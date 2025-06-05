(*  Terminology:
    {b 'ballot'} is a single voter's ballot listing the voter's preferences
    {b 'vote'} is a single voter's vote; it is assigned to the candidates that
    are shown by the voter's preferences according to each preferred candidate's keep value.
    Ballots are represented as a sequence of integer lists. Each list
    contains candidate ids in order of preference. The first element of
    the list is the number of ballots having that order of preference. Thus
    [63; 4; 2; 1; 3] represents 63 ballots, each having preferences 4, 2, 1, 3.  *)

open Format

type can_status = Hopeful | Successful | Excluded | Withdrawn
type cand = 
| Name of string
| Status of can_status
| Keep of float
| Votes of float
| Ahead of int
| PRNG of int

(* *********************************
             Settings
   ********************************* *)
(** Debugging flag for transfer function *)
let debug_transfer = false

(** Maximum iterations  *)
let max_iters = 200


(* *********************************
             Printers
   ********************************* *)

(** Print an integer list  *)
let pp_int_list fmt ls =
  fprintf fmt "[";
  List.iteri (fun i x ->
    if i > 0 then fprintf fmt "; ";
    fprintf fmt "%d" x) ls;
  fprintf fmt "]"

(** Print candidate data  *)
let pp_candidate fmt candidate =
  match candidate with
  | (id, Name n, Status s, Keep k,Votes v, Ahead a, PRNG p) -> 
    fprintf fmt "(%d, %S, %s, %.4f, %.4f,%d,%d)" id n (
      begin
      match s with | Hopeful -> "Hopeful" | Successful -> "Successful" | Excluded -> "Excluded" | Withdrawn -> "Withdrawn"
      end) k v a p
  | _ -> fprintf fmt "error"

(** Print a candidate table  *)
let pp_table fmt candidates =
  print_flush ();
  open_vbox 0;
  open_tbox ();
  fprintf fmt "  "; set_tab (); 
  printf  "Index "; set_tab (); 
  printf  "Name                  "; set_tab (); 
  printf  "Status      "; set_tab (); 
  printf  "%8s   " "Keep"; set_tab (); 
  printf  "%14s" "Votes"; set_tab ();
  printf  "%9s" "Ahead"; set_tab ();
  printf  "%10s" "PRNG"; 
  let delim = String.init 84 (fun _ -> '-') in
  printf "\n  %s" delim;
  List.iter (fun cand -> 
    match cand with
    | (id,Name n,Status s,Keep k,Votes v,Ahead a,PRNG p) ->
      print_tab ();
      printf "%d" id;
      print_tab ();
      printf "%s" (if String.length n > 17 then String.sub n 0 17 else n);
      print_tab ();
      printf "%s" (match s with | Hopeful -> "Hopeful" | Successful -> "Successful" | Excluded -> "Excluded" | Withdrawn -> "Withdrawn"); 
      print_tab ();
      printf "%f" k;    
      print_tab ();
      printf "%14f" v;
      print_tab ();
      printf "%9d" a;
      print_tab ();
      printf "%9d" p;
    | _ -> printf "Error";  
  ) candidates;
  close_tbox ();
  close_box ();
  print_flush ()

(** Function to print a report of all candidates
    @param iter the current iteration
    @param num the report number
    @param candidates the list of candidates
    @param quota the current quota
    @param surplus the current surplus
    @param ntv the current number of non-transferable votes *)
let report ~iter ~num ~candidates ~quota ~surplus ~ntv = 
  let heading =
    match num with
    | 1 -> "Candidates achieving quota changed to Successful"
    | 2 -> "Candidates with keep values"
    | 3 -> "Candidates after allocating votes"
    | 4 -> "Hopeful candidates achieving quota upgraded to Successful"
    | 5 -> "Candidates at end of iteration"
    | x -> sprintf "Report number: %s" (string_of_int x)
  in
  printf "Iteration: %d    %s\n\n" iter heading;
  (* printf "  %s\n\n" heading; *)
  printf "  Current quota: %f\n\n" quota;
  printf "%a\n\n" pp_table candidates;
  printf "  Total surplus: %f\n" surplus;
  printf "  Non-transferable votes: %f\n\n@." ntv

(** Function to print Local Electoral Regulations 2001 clause as a heading  *)
let print_clause_heading description = 
    (* let separator = "-------------------------------------------------------" in *)
    let separator = "*******************************************************" in
    printf "%s\n" separator;
    printf "* %s\n" description;
    printf "%s\n" separator


(* *********************************
             File reader
   ********************************* *)

(** Reads a blt file *)
let read_blt_file filename =
  let chan = open_in filename in

  (* Helper to test whether a string can be converted to an int *)
  let is_int s =
    try ignore (int_of_string s); true
    with Failure _ -> false
  in

  (* Helper to get the next line, lazily *)
  let line_seq  =
    Seq.unfold (fun _ ->
      try Some (input_line chan, ()) with End_of_file -> None
    ) ()
  in

  (* Split the file into ballot and candidate lines *)
  let ballot_lines, candidate_lines =
    let rec split acc = function
      | Seq.Cons (line, rest) when String.starts_with ~prefix:"#" line -> split acc (rest ()) (* Skip any line starting with '#' *)
      | Seq.Cons ("0", rest) -> List.to_seq (List.rev acc), rest
      | Seq.Cons (line, rest) -> split (line :: acc) (rest ())
      | Seq.Nil -> List.to_seq (List.rev acc), Seq.empty
    in
    split [] (line_seq ())
  in

  (* The first line of ballot_lines is the info about number of seats and candidates *)
  let candseats, ballots = 
    match Seq.uncons ballot_lines with
    | Some (h,t) -> h,t
    | _ -> failwith "Error extracting number of candidates and seats"
  in

  (* Ballot sequence: filter for lines starting with integer, convert to a sequence of integer lists *)
  let ballot_seq =
    ballots
    |> Seq.filter_map (fun line ->
         let tokens = String.split_on_char ' ' line |> List.filter ((<>) "") in
         match tokens with
         | first :: _ when is_int first ->
             Some (List.take (List.length tokens - 1) (List.map int_of_string tokens)) (* Return the list only if it starts with an integer and drop the end zero *)
         | _ -> None
       )
  in

  (* Candidate sequence: for each line, drop quotes and trim it *)
  let candidate_seq =
    candidate_lines
    |> Seq.take_while (fun line -> String.trim line <> "\"\"")
    |> Seq.map (fun line -> String.trim (String.sub line 1 (String.length line - 2)))
  in
  candseats, ballot_seq, candidate_seq


(* *********************************
             Helpers
   ********************************* *)

let votes_of_candidate = function
| (_,_,_,_,Votes v,_,_) -> v
| _ -> 0.

let index_of_candidate = function
| (a,_,_,_,_,_,_) -> a

let ahead_of_candidate = function
| (_,_,_,_,_,Ahead a,_) -> a
| _ -> 99999

let create_candidates candidates_blt = 
  List.mapi (fun i cand -> (i + 1,Name cand,Status Hopeful,Keep 1.0,Votes 0.,Ahead 0,PRNG 0)) candidates_blt

let num_ballots ballots = Seq.fold_left (fun acc b_line -> acc + List.hd b_line) 0 ballots

(** The initial calculation of Ahead values. Sorts the candidates by 
    their votes then assigns an Ahead value in increments of 2 as per 
    Hill. In doing so it tracks ties. A tie is assigned the same value. *)
let calculate_ahead cands =
  let sorted_candidates_by_vote = List.sort (fun cd1 cd2 -> compare (votes_of_candidate cd1) (votes_of_candidate cd2)) cands in
  let sorted_candidates_by_vote_with_ahead =
    let rec cands_ahead candidates updated acc votes ties = 
      begin match candidates with
      | [] -> updated
      | (a,Name b,Status c,Keep d,Votes e,_,PRNG g) :: tl -> 
          if e <> votes then cands_ahead tl ((a,Name b,Status c,Keep d,Votes e,Ahead acc,PRNG g)::updated) (acc + 2) e 0
          else cands_ahead tl ((a,Name b,Status c,Keep d,Votes e,Ahead (acc - 2*(ties+1)),PRNG g)::updated) (acc + 2) e (ties + 1)
      | _ -> printf "Error calculating Ahead!!!!";candidates
      end
    in
    cands_ahead sorted_candidates_by_vote [] 0 0. 0
  in
  List.sort (fun cd1 cd2 -> compare (index_of_candidate cd1) (index_of_candidate cd2)) sorted_candidates_by_vote_with_ahead

(** Checks if any candidates with tied Ahead values are now separated.  *)
let check_ties cands = 
  let sorted_by_ahead cds = List.sort (fun cd1 cd2 -> compare (ahead_of_candidate cd1) (ahead_of_candidate cd2)) cds in
  let rec examine cds acc tie_found = 
    match cds with
    | [] -> acc
    | (a1,b1,c1,d1,Votes e1, Ahead f1,g1) :: (a2,b2,c2,d2,Votes e2, Ahead f2,g2) :: tl ->
        if f1 = f2 then 
          if e1 > e2 then examine ((a1,b1,c1,d1,Votes e1, Ahead (f1+2),g1) :: tl) ((a2,b2,c2,d2,Votes e2, Ahead f2,g2) :: acc) true
          else if e1 < e2 then examine ((a2,b2,c2,d2,Votes e2, Ahead (f2+2),g2) :: tl) ((a1,b1,c1,d1,Votes e1, Ahead f1,g1) :: acc) true
          else examine ((a2,b2,c2,d2,Votes e2, Ahead f2,g2) :: tl) ((a1,b1,c1,d1,Votes e1, Ahead f1,g1):: acc) true
        else examine ((a2,b2,c2,d2,Votes e2, Ahead f2,g2) :: tl) ((a1,b1,c1,d1,Votes e1, Ahead f1,g1):: acc) tie_found
    | [(a1,b1,c1,d1,Votes e1, Ahead f1,g1)] -> 
      if tie_found then examine (sorted_by_ahead ((a1,b1,c1,d1,Votes e1,Ahead f1, g1) :: acc)) [] false 
      else ((a1,b1,c1,d1,Votes e1,Ahead f1, g1) :: acc)
    | _ -> acc
      in
    examine (sorted_by_ahead cands) [] false

let assign_first_prefs ballots candidates = 
  Seq.fold_left (fun acc b -> 
    (* Get number of votes to go to candidate *)
    let num_votes = float_of_int (List.nth b 0) in
    (* Get candidate id *)
    let cid = List.nth b 1 in
    (* Get index in sequence for this candidate *)
    let candidate_idx = 
      let idx_opt = List.find_index (fun (a,_,_,_,_,_,_) -> a = cid) candidates in
      begin match idx_opt with Some x -> x | None -> failwith "Error getting candidate index" end
    in
    (* let candidate_opt = Seq.find (fun (a,_,_,_,_,_,_) -> a = cid) acc in
    let candidate = begin match candidate_opt with | Some c -> c | None -> failwith "Here----" end in *)
    let candidates_updated = 
      let candidate_updated c = 
        begin match (c: int * cand * cand * cand * cand * cand * cand) with
        | (a,b,c,d,Votes e,f,g) -> (a,b,c,d,Votes (e +. num_votes),f,g)
        | _ -> c
        end  
      in
      List.mapi (fun i c -> if i = candidate_idx then candidate_updated c else c) acc in
    (* Put updated candidates into acc *)
    candidates_updated
    ) candidates ballots 

(** Works through the preferences of a single ballot, identifying 
    the candidate and allocating a vote, or part vote, to the candidate. When list of
    preferences is empty, any remaining vote value is added to non-transferable votes
    @param ballot a ballot
    @param (candidates,ntv) the list of candidates and current non-transferable votes
    @param voteval tracks current value of the ballot  *)
let rec transfer num_votes prefs (candidates,ntv) voteval = 
  let () = if debug_transfer then 
    printf "------- Num_votes: %f; Prefs: %a; ntv: %f -------\n\n" num_votes pp_int_list prefs ntv in
  match prefs with
  | [] -> (candidates, ntv +. (voteval *. num_votes))
  | hd :: tl -> 
    (* Get the candidate for this preference *)
    let cd = List.find (fun (a,_,_,_,_,_,_) -> a = hd) candidates in
    (* Log this candidate *)
    let () = if debug_transfer then printf "Candidate before allocation of vote:  %a\n" pp_candidate cd in
    let () = if debug_transfer then printf "Vote value before allocation of vote:  %f\n" voteval in
    (* Allocate vote to this candidate *)
    let (cd2,vv) = 
      begin match cd with
        | (a,b,c,Keep d,Votes e,Ahead f,PRNG g) -> ((a,b,c, Keep d, Votes (e +. (voteval *. d *. num_votes)),Ahead f,PRNG g),((1. -. d) *. voteval))
        | _ -> (cd, 99.)
      end
    in
    let () = if debug_transfer then printf "Candidate after allocation of vote:   %a\n" pp_candidate cd2 in 
    let () = if debug_transfer then printf "Vote value after allocation of vote:  %f\n\n" vv in

    (* Update list of all candidates with this changed candidate *)
    let candidates2 = List.map (fun cd -> 
      let (id,_,_,_,_,_,_) = cd2 in
      begin match cd with
      | (a,_,_,_,_,_,_) when a = id -> cd2 
      | _ -> cd
      end
      ) candidates     
    in
    transfer num_votes tl (candidates2,ntv) vv

(** Recursively works through the list of ballots, calling on 
    [transfer] for each ballot. It returns a tuple containing 
    a new list of candidates and the number of non-transferable votes.  *)
let allocate_ballots ballots candidates ntv = 
  Seq.fold_left (fun acc b -> 
    let (cands,ntv) = transfer (float_of_int (List.hd b)) (List.tl b) acc 1. in
    cands,ntv   
    ) (candidates,ntv) ballots
  
    
let end_election cands = 
  let () = printf "**** End of election ****\n\n" in
  let () = printf "Elected:\n" in
  let () = List.iter (fun cand -> 
    begin match cand with
    |(_,Name b,Status Successful,_,_,_,_) -> printf "  %s\n" b
    |_ -> printf ""
    end
    ) cands   
  in
  let () = printf "\n\n" in
  cands

(* *********************************
             Main function
   ********************************* *)

(** The main function follows the caculation steps set out in 
    https://legislation.govt.nz/regulation/public/2001/0145/latest/DLM57125.html. *)


let run candseats ballots candidate_names debug =
  
  (* If not enough candidates *)
  (* if IMap.cardinal choices <= nseats then
    choices |> IMap.bindings |> List.map fst
    |> (fun x -> `Win x :: events)
    |> List.rev
  
  else *)

  (* Print the title of the election *)

  let num_cands = List.nth candseats 0 in
  let num_seats = List.nth candseats 1 in

  (* Number of ballots without any preferences *)
  let num_informal_ballots = 
    let first = match Seq.uncons ballots with 
      | Some (h,_) -> h
      | None -> failwith "Error getting first element in sequence"
    in 
    if List.length first = 1 then List.hd first
    else 0     
  in
  let ballots_formal = if num_informal_ballots > 0 then 
    Seq.drop 1 ballots else ballots 
  in
  let candidates = create_candidates candidate_names in
  let total_votes = num_ballots ballots in

  let () = printf "Seats: %d\nNumber of candidates: %d\n" num_seats num_cands in
  let () = printf "Informal votes: %d\n" num_informal_ballots in
  let () = printf "Total votes: %d\n\n" total_votes in
  let () = if debug then printf "Candidates at start:\n\n%a\n\n" pp_table candidates in


(* --------------------------- Step 1 -------------------------- *)

  let quota = ((float_of_int (num_ballots ballots) -. float_of_int num_informal_ballots) /. (Float.of_int (num_seats + 1))) +. 0.000000001 in
  let candidates_with_first_prefs = assign_first_prefs ballots_formal candidates in

  let firstpref_winners quota = 
    (* Find candidates equal or over quota *)
    List.filter (fun cand -> match cand with
      | (_,_,_,_,Votes e,_,_) -> e >= quota
      | _ -> false) 
      candidates_with_first_prefs
  in
  let successful_candidates = firstpref_winners quota in
   
  let () = if debug then begin
    print_clause_heading "Clause 5: Calculate quota";
    printf "  Quota is: %.9f\n\n" quota;
    print_clause_heading " Clause 6: Candidates reaching quota become Successful";
    printf "Candidates with first preferences allocated:\n\n%a\n\n" pp_table candidates_with_first_prefs;
    printf "Successful candidates:\n\n%a\n\n" pp_table successful_candidates 
  end in

  (* Change the status of any candidates reaching the quota to Successful *)
  let cands = 
    (* If any candidate at quota or above then change their status to Successful *)      
    if List.length successful_candidates > 0 then
      let candidates2 = List.fold_left (fun acc (a,b,c,d,e,f,g) -> 
        if List.exists (fun (x,_,_,_,_,_,_) -> x = a) successful_candidates then (a,b,Status Successful,d,e,f,g) :: acc 
        else (a,b,c,d,e,f,g) :: acc ) 
        [] candidates_with_first_prefs
      in
      candidates2
    else
      (* No candidates at quota or above *)
      let () = if debug then printf "There are no candidates at quota or above\n" in
      candidates_with_first_prefs
  in

(* ---------------------- Calculate Ahead values ------------------ *)

let cands = calculate_ahead cands in
let () = if debug then 
  report ~iter:0 ~num:1 ~candidates:cands ~quota:quota ~surplus:0. ~ntv:0. in

(* ====================== Step 2 and onwards ====================== *)

 let rec iteration ballots candidates quota iter = 

(* ----- Check if all seats filled or reached max iterations -------*)

    let successful = List.filter (fun (_,_,c,_,_,_,_) -> c = Status Successful) candidates in
    if List.length successful = num_seats then end_election candidates
    else if iter >= max_iters then candidates
    else

    let () = if debug then printf "=============== Entering iteration : %d ===============\n\n" iter in

(* ---------------------- Set keep values ------------------------- *)

    let () = if debug then print_clause_heading "Clauses 7, 8, 9: Calculate keep values" in
      
    (* Give each successful and withdrawn candidate a Keep Value *)
    let kv_for_successful_cand cid = 
      (* Get candidate from index value *)
      let candidate = List.find (fun (a,_,_,_,_,_,_) -> a = cid) candidates in
      (* Get this candidate's current votes *)
      let (kv,votes) = 
        match candidate with
        | (_,_,_,Keep k,Votes v,_,_) -> (k,v)
        | _ -> (1.,1.)
      in
        (* Return keep value *)
      (quota /. votes) *. kv
    in
    
    let updated_candidates = List.map (fun (candidate) -> 
        begin match candidate with
        | (a,Name b,Status Successful, _, Votes v,Ahead f,PRNG g) -> 
            let kv = kv_for_successful_cand a in (a,Name b,Status Successful,Keep kv,Votes (v *. kv),Ahead f, PRNG g)
        | (a,Name b,Status Excluded, _, Votes v,Ahead f, PRNG g)-> (a,Name b,Status Excluded,Keep 0.,Votes v,Ahead f, PRNG g)
        | (a,Name b,Status Withdrawn, _, Votes v,Ahead f, PRNG g)-> (a,Name b,Status Withdrawn,Keep 0.,Votes v,Ahead f, PRNG g)          
        | (a,Name b,Status Hopeful,_,Votes v,Ahead f, PRNG g) -> (a,Name b,Status Hopeful,Keep 1.,Votes v,Ahead f, PRNG g)
        | (_,_,_,_,_,_,_) -> candidate
        end
      ) candidates      
    in
    let () = if debug then report ~iter:iter ~num:2 ~candidates:updated_candidates ~quota:quota ~surplus:0. ~ntv:0. in

(* ---------------------- Allocate votes ---------------------- *)

    let reset_candidates_votes = List.map (fun (candidate) ->
      begin match candidate with
      | (a,Name b,Status c,Keep d,_,Ahead f,PRNG g) -> (a,Name b,Status c,Keep d,Votes 0.,Ahead f,PRNG g) 
      | _ -> candidate
      end
      ) updated_candidates in

    let (cands,ntv) = allocate_ballots ballots reset_candidates_votes 0. in

    let () = if debug then begin
      print_clause_heading " Clause 10: Allocate votes";
      report ~iter:iter ~num:3 ~candidates:cands ~quota:quota ~surplus:0. ~ntv:ntv 
    end in

(* ---------------------- Calculate new quota ------------------------- *)

    let nballots = (Float.of_int (num_ballots ballots)) in
    let quota = ((nballots -. ntv)/. (Float.of_int (num_seats + 1))) +. 0.000000001 in

    let () = if debug then begin
      print_clause_heading " Clause 11: Calculate new quota";
      printf "\nNumber of ballots:      %f\n" nballots;
      printf "Non-transferable votes: %f\n" ntv;
      printf "New quota is:           %.9f\n\n" quota 
    end 
    in

    (* If no candidates at quota then end election *)

(* ---------------------- Hopeful candidates that meet quota become Successful ----------- *)

    let upgrade_hopeful = 
      let updated_candidates = List.fold_left (fun acc candidate ->
        match candidate with
        | (a,Name b,Status Hopeful,Keep d,Votes e,Ahead f, PRNG g) when e >= quota -> (a,Name b,Status Successful,Keep d,Votes e,Ahead f, PRNG g) :: acc
        | _ -> candidate :: acc
        ) [] cands in
      List.rev updated_candidates
    in
    let cands = upgrade_hopeful in

    let () = if debug then begin
      print_clause_heading " Clause 12: Hopeful candidates who meet quota become successful";
      report ~iter:iter ~num:4 ~candidates:cands ~quota:quota ~surplus:0. ~ntv:ntv 
      end 
    in

(* ----------- Exclude the hopeful candidate with the least votes 
                if the sum of his or her votes and the total surplus 
                is less than the votes of any other hopeful candidate 
                or if the total surplus is less than 0.0001 ----------- *)

    let surplus = List.fold_left (fun acc candidate ->
      match candidate with
      | (_,_,_,_,Votes e,_,_) when e > quota -> acc +. (e -. quota)
      | _ -> acc
      ) 0. cands in

    let ec_remove_lowest_hopeful = 
      let hopeful_candidates = List.filter (fun (_,_,c,_,_,_,_) -> c = Status Hopeful) cands in
      let sorted_candidates = List.sort (fun cd1 cd2 -> compare (votes_of_candidate cd1) (votes_of_candidate cd2)) hopeful_candidates in
      
      let () = if debug then begin
        print_clause_heading "Clause 13: Exclude lowest hopeful if criteria met"; 
        printf "Hopeful candidates sorted by votes:\n\n%a\n\n" pp_table sorted_candidates
      end
      in

      (* Get the candidate with the least votes *)
      let excluded_candidate = 
        begin match sorted_candidates with
        | hd :: _ when surplus < 0.0001 -> if debug then printf "No surplus remains, so lowest candidate must be excluded\n";Some hd
        | (a,Name b,Status c,Keep d,Votes e,Ahead f,PRNG g) :: tl -> 
          begin match tl with
          | (_,_,_,_,Votes j,_,_) :: _ when e +. surplus < j -> 
            if debug then printf "Lowest candidate cannot overtake, so can safely be excluded\n"; Some (a,Name b,Status c,Keep d,Votes e,Ahead f, PRNG g)
          | _ -> None
          end
        | _ -> None
        end

      in
      let () = match excluded_candidate with
        | Some candidate -> if debug then printf "Candidate to be excluded: %a\n" pp_candidate candidate 
        | _ -> if debug then printf "  No candidate able to be excluded.\n"
      in
      (* Update candidates list by marking any excluded candidate as Excluded *)
      let updated_candidates = List.map (fun candidate -> 
        match candidate with
        | (a,Name b,Status c,Keep d,Votes e,Ahead f,PRNG g) when excluded_candidate = Some (a,Name b,Status c,Keep d,Votes e,Ahead f,PRNG g) -> (a,Name b,Status Excluded,Keep 0.,Votes 0.,Ahead f,PRNG g)
        | _ -> candidate
      ) cands in
      
      updated_candidates

    in
    let cands = ec_remove_lowest_hopeful in
    let cands = check_ties cands in
    let () = report ~iter:iter ~num:5 ~candidates:cands ~quota:quota ~surplus:surplus ~ntv:ntv in 

    iteration ballots cands quota (iter + 1)
    in
  iteration ballots cands quota 1


(* *********************************
  Command line parsing to read a blt file
   ********************************* *)
let usage_msg = "Usage: nzstv FILENAME"
let usage_msg2 = "FILENAME missing!\n" ^ usage_msg
let filename = ref ""
let verbose = ref false
let speclist = [("-v", Arg.Set verbose, "Output debug information");]

let candseats, blt_ballots, blt_candidates =
  Arg.parse speclist (fun x -> filename := x) usage_msg;
  if String.length !filename < 1 then begin Arg.usage speclist usage_msg2; exit 1; end;
  read_blt_file !filename

(* *********************************
        Print election title
   ********************************* *)

(* Candidates are represented as a list of tuples and ballots as a sequence of integer lists *)
let title, candidates = 
  let list = List.of_seq blt_candidates in
  let cand_list = List.take (List.length list - 1) list in
  let title = List.drop (List.length list - 1) list in
  List.hd title, cand_list
  
let () = Printf.printf "\nTitle: %s\n" title 
let () = if !verbose then begin
  printf "\nCandidates/seats: %s\n" candseats; 
  printf "First 10 ballot lines:\n";
  Seq.iteri (fun i el -> if i < 10 then (printf "%a\n" pp_int_list el)) blt_ballots; 
  printf "\n"
end

(* *********************************
        Run the main function
   ********************************* *)
let _ = run (List.map int_of_string (String.split_on_char ' ' candseats)) blt_ballots  candidates !verbose



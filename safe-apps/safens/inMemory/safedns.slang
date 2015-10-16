/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @author Vamsi Thummala {vamsi@cs.duke.edu}, Copyright (C) 2013-2015
 *
 */


//import('extractAttribute').

resolvePredicateAttr(?SetRef, ?Speaker, ?Subject, ?SubjectSetRef) :-
  spec('
    find the srn predicate from the SetRef context and extract 
    the set reference for subsequent fetch
  '),
  (* lsetToSeq? converts the slog results to a slang sequence *)
  ?AttrSeq := transposeToSeq(queryPredicate(?SetRef, ?Speaker, ?Subject)),
  getAttribute(?AttrSeq, ?Speaker, ?Subject, ?SubjectSetRef)
end
  
getAttribute([?Head|?Tail], ?Speaker, ?Subject, ?SubjectSetRef) :-
  spec('
    extract the set reference attribute value from the 
    first result of slog output sequence
  '),
  ?Head = srn(?Speaker, ?Subject, ?SubjectSetRef)
end

defguard queryPredicate(?SetRef, ?Speaker, ?Subject) :-
  spec('query the required predicate from the set context'),
  {
    import($SetRef).
    $Speaker: srn($Subject, ?SubjectSetRef)?
  }
end

(* Program to resolve a path sequence starting from a known
 * root reference and accumulate the relavant context for 
 * later validation through slog
 *)
resolveContextIterative(?RootRef, ?Path, ?ImportSeq) :-
  spec('route to helper method with an accumulator'),
  ?PathSeq := toSeq(?Path), // convert the path to slang sequence
  resolveContextIterativeHelper(?RootRef, ?PathSeq, [?RootRef], ?ImportSeq)
end

(* terminating condition for recursion where ?PathSeq is a leaf;
 * unify ?Accumulator with ?ImportSeq
 *)
resolveContextIterativeHelper(?RootRef, [?Leaf], ?ImportSeq, ?ImportSeq).

resolveContextIterativeHelper(
  ?RootRef,
  [?RootName, ?ChildName|?Tail],
  ?Accumulator,
  ?ImportSeq
) :-
  spec('
    extract the ?ChildSetRef from the ?RootRef context and 
    loop until ?ChildName is nil; accumulate the ?ChildSetRef
  '),
  resolvePredicateAttr(?RootRef, ?RootName, ?ChildName, ?ChildSetRef),
  resolveContextIterativeHelper(
    ?ChildSetRef,
    [?ChildName|?Tail],
    [?ChildSetRef|?Accumulator],
    ?ImportSeq
  )
end


(* Evaluate the accumulated context to verify
 * the integrity of the path.
 *)
defguard dns(?Name, ?Address, ?RootRef) :-
  resolveContextIterative(?RootRef, ?Name, ?ImportRef),
  ?NameAtRoot := head(?Name),
  ?NameAtLeaf := last(?Name),
  {
    import($ImportRef).

    dns(?Name, ?Address) :-
      validateName($NameAtRoot, $NameAtLeaf, ?Name),
      _: aRecord(?Name, ?Address)
    end

    (* Terminating condition for recursion where 
     * the root == leaf and name is nil 
     *)
    validateName($NameAtLeaf, $NameAtLeaf, nil).

    validateName(?Speaker, ?NameAtLeaf, ?Name) if
      domainTerm(?Name), // this constraint is required to 
                         // ensure ?Name is grounded or else
                         // range check will fail
      ?Subject := head(?Name),
      ?Rest    := tail(?Name),
      ?Speaker: srn(?Subject, _),
      validateName(?Subject, ?NameAtLeaf, ?Rest)
    end

    dns($Name, ?Address)?
  }
end

defcon buildSRN(?SetName, ?Speaker, ?Subject) :-
  ?SubjectSetRef := concat(?Subject, 'Root'),
  {
    name("$SetName").
    $Speaker: srn($Subject, $SubjectSetRef).
  }
end

(*
'.': srn('.', '.Root').
'.': srn('edu', 'eduRoot').
'.': srn('com', 'comRoot').
'.': srn('org', 'orgRoot).

'com': srn('amazon', 'amazonRoot').
*)

defcon addRecord(?SetName, ?Speaker, ?SRN, ?IpAddress) :-
  {
    name("$SetName").
    $Speaker: aRecord($SRN, $IpAddress).
  }
end


// cs.duke.edu. | . -> edu -> duke -> cs

definit
  buildSRN('.Root', '.', '.'), // this is a self-certified SRN by '.', which needs to be trusted by the authorizer
  buildSRN('.Root', '.', 'edu'), buildSRN('.Root', '.', 'com'), buildSRN('.Root', '.', 'org'), buildSRN('.Root', '.', 'gov'),
  buildSRN('comRoot', 'com', 'amazon'), buildSRN('comRoot', 'com', 'google'), buildSRN('comRoot', 'com', 'yahoo'),
  buildSRN('orgRoot', 'org', 'safeclouds'), buildSRN('orgRoot', 'org', 'eff'), buildSRN('orgRoot', 'org', 'code'),
  buildSRN('eduRoot', 'edu', 'duke'), buildSRN('eduRoot', 'edu', 'unc'), buildSRN('eduRoot', 'edu', 'ncstate'),
  buildSRN('googleRoot', 'google', 'mail'), buildSRN('googleRoot', 'google', 'play'), buildSRN('googleRoot', 'google', 'search'),
  buildSRN('dukeRoot', 'duke', 'cs'), buildSRN('dukeRoot', 'duke', 'math'), buildSRN('dukeRoot', 'duke', 'ece'),
  addRecord('dukeRoot', 'duke', dn"cs.duke.edu", '172.168.0.1'),
  dns(dn"cs.duke.edu", ?Address, '.Root')
end

// Output upon execution
(*

  query() :- '.Root', '.Root', '.Root', '.Root', '.Root', 'comRoot', 'comRoot', 'comRoot', 'orgRoot', 'orgRoot', 'orgRoot', 'eduRoot', 'eduRoot', 'eduRoot', 'googleRoot', 'googleRoot', 'googleRoot', 'dukeRoot', 'dukeRoot', 'dukeRoot', 'dukeRoot', {
    $Self: dns(dn"cs.duke.edu", '172.168.0.1').
  }.

*)

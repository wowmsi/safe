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



import!('extractAttribute').

defenv Dot :- u'f-spKkzyR84ZtwaKwyhiYBKX5PF34bIZnfDGPpHKwWw'.

defenv ARecordSetName :- 'aRecord'.

(** 
 * Program to resolve a path sequence starting from a known
 * root reference and accumulate the relavant context for 
 * later validation through slog
 *)
resolveSRN(?RootRef, ?Path, ?ImportSeq) :-
  spec('route to helper method with an accumulator'),
  ?PathSeq := toSeq?(?Path), // convert the path to slang sequence
  resolveSRNHelper(?RootRef, ?PathSeq, [], ?ImportSeq),
end

(* terminating condition for recursion where ?PathSeq is a leaf;
 * unify ?Accumulator with ?ImportSeq
 *)
resolveSRNHelper(?Leaf, [?LeafCN], ?Accumulator, ?ImportSeq) :-
  ?LeafSetRef := computeIdFromName?(?Leaf, $ARecordSetName),
  ?ImportSeq   = [?LeafSetRef|?Accumulator],
end

resolveSRNHelper(
  ?ParentRef,
  [?ParentName, ?ChildName|?Rest],
  ?Accumulator,
  ?ImportSeq,
) :-
  spec('
    extract the ?ChildSetRef from the ?ParentRef context and 
    loop until ?ChildName is nil; accumulate the ?ChildSetRef
  '),
  ?Parent       := rootId?(?ParentRef),  
  ?ParentSetRef := computeIdFromName?(?Parent, ?ChildName),
  resolvePredicateAttr(?Parent, ?ParentSetRef, ?ChildName, ?ChildSetRef),
  resolveSRNHelper(
    ?ChildSetRef,
    [?ChildName|?Rest],
    [?ChildSetRef, ?ParentSetRef|?Accumulator],
    ?ImportSeq
  ),
end

(* Evaluate the accumulated context to verify
 * the integrity of the path.
 *)
defguard dnsQuery?(?Name, ?Address, ?RootRef) :-
  resolveSRN(?RootRef, ?Name, ?ImportRef),
  ?NameAtRoot := head?(?Name),
  ?NameAtLeaf := last?(?Name),
  [?LeafSetRef, ?LeafRef|?Rest] = ?ImportRef,
  ?Leaf       := rootId?(?LeafRef),
  _           := fetch?(?ImportRef),
  {
    import!($ImportRef).

    dns(?Name, ?Address) :-
      validateName($RootRef, ?Name),
      $Leaf: aRecord(?Name, ?Address),
    end

    (* Terminating condition for recursion where 
     * the speaker == leaf
     *)
    validateName($Leaf, ?Name) :-
      ?LeafCN := head?(?Name),
      ?LeafCN  = $NameAtLeaf,
    end

    validateName(?SpeakerRef, ?Name) :-
      ?Speaker   := rootId?(?SpeakerRef),
      ?SpeakerCN := head?(?Name),
      ?Rest      := tail?(?Name),
      ?SubjectCN := head?(?Rest),
      ?Speaker: srn(?Subject, ?SubjectCN),
      validateName(?Subject, ?Rest),
    end

    dns($Name, ?Address)?
  },
end

definit dnsQuery?(dn"cs.duke.edu", ?Address, $Dot), dnsQuery?(dn"cs.duke.com", ?Address, $Dot)?

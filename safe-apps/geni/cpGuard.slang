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



defenv Selfie :-
  spec('Load the principal keypair'),
  principal($1).
end

defenv GeniRoot :- $2.
defenv Idp      :- $3.
defenv PA       :- $4.
defenv SA       :- $5.
defenv CP       :- $6.
defenv PI       :- $7.
defenv User1    :- $8.
defenv User2    :- $9.

defenv EndorsePISet    :- computeIdFromName($Idp, "endorse/pi/$PI").
defenv EndorseUserSet  :- computeIdFromName($Idp, "endorse/user/$User1").
defenv ProjectRef      :- "$PI:projectGuid".
defenv ProjectSet      :- computeIdFromName($PA,  "capability/?ProjectRef/$PI").
defenv SliceRef        :- "$SA:sliceGuid".
defenv SliceSet        :- computeIdFromName($SA,  "delegate/$User1/?SliceRef").
defenv TrustStructure  :- computeIdFromName($Self, 'subject/trust-structure').
defenv RegisteredUser  :- computeIdFromName($Self, 'subject/registered-user').
defenv GeniEndorsement :- computeIdFromName($GeniRoot, 'endorseAll').
defenv CPPolicySet     :- computeIdFromName($Self, 'object/aggregate-policy-slice-operations').

defetch fetchAllLinks() :- 
  [?EndorsePISet, ?EndorseUserSet, ?ProjectSet, ?SliceSet, ?TrustStructure, ?RegisteredUser, ?GeniEndorsement].

defcon aggregatePolicySet() :- 
  {
     sliverInstantiate(?Slice, ?Subject, ?Project) :-
       ?SA := rootId(?Slice),
       ?SA: slice(?Slice, ?Project, standard),
       sliceAuthority(?SA), 
       ?SA: controlPrivilege(?Subject, ?Slice, instantiate, _),
       geniUser(?Subject).
 
     name('object/aggregate-policy-slice-operations').
  }.

defguard authorize() :-
  spec('authorize the subject to access the slice on given project'),
  ?Subject        := "$User1",
  ?FetchedSets    := fetchAllLinks(),
  {
     import($FetchedSets).

     sliverInstantiate(?Slice, ?Subject, ?Project) :-
       ?SA := rootId(?Slice),
       //println('printing SA'),
       //println(?SA),
       ?SA: slice(?Slice, ?Project, standard),
       //println('printing Slice'),
       //println(?Slice),
       sliceAuthority(?SA), 
       ?SA: controlPrivilege(?Subject, ?Slice, instantiate, _),
       geniUser(?Subject).

     geniRoot($GeniRoot).
 
     sliverInstantiate($SliceRef, $Subject, $ProjectRef)?
  }
end

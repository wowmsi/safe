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

defcon standardSliceControlSet() :- 
  {
    controls(?Subject, ?Slice, true) :-
      owner(?Subject, ?Slice).
 
    controls(?Subject, ?Slice, ?Delegatable) :- 
      ?Delegator: delegateControl(?Subject, ?Slice, ?Delegatable),
      controls(?Delegator, ?Slice, true).
 
    controls(?Subject, ?Slice) :-
      controls(?Subject, ?Slice, _).
 
    controlPrivilege(?Subject, ?Slice, instantiate, ?Delegator) :- 
      controls(?Subject, ?Slice, ?Delegator).
 
    controlPrivilege(?Subject, ?Slice, ?Priv, ?Delegatable) :- 
      ?Delegator: delegateControlPrivilege(?Subject, ?Slice, ?Priv, ?Delegatable),

    controlPrivilege(?Delegator, ?Slice, ?Priv, true).
 
    name('object/standardSliceControl') :- 
      spec('Object set for standard slice control policy issued by self').
  }.

defcon standardSliceDefaultPrivilegeSet() :-
  {
    controlPrivilege(?Subject, ?Slice, info, ?Delegatable) :-
      slice(?Slice, ?Project, standard),
      ?PA := rootId(?Project),
      projectAuthority(?PA),
      ?PA: project(?Project),
      ?PA: memberPrivilege(?Subject, ?Project, info, ?Delegatable).
 
    controlPrivilege(?Subject, ?Slice, stop, true) :- 
      slice(?Slice, ?Project, standard),
      ?PA := rootId(?Project),
      projectAuthority(?PA),
      ?PA: project(?Project),
      owner(?Subject, ?Project).
 
    controlPrivilege(?Subject, ?Slice, info) :- 
      slice(?Slice, ?Project, standard),
      gmoc(?Subject).
 
    controlPrivilege(?Subject, ?Slice, stop) :- 
      slice(?Slice, ?Project, standard),
      gmoc(?Subject).
 
    name('object/standardSliceDefaultPrivilege') :- 
      spec('Object set for standard slice default privilege policy issued by self').
  }.

defcon createSlice(?SubjectId, ?SliceId, ?ProjectRef, ?StandardSliceControlRef, ?StandardSliceDefaultPrivilegeRef) :-
  {
    owner($SubjectId, $SliceId).
    slice($SliceId, $ProjectRef, standard).
    link($StandardSliceControlRef) :- spec('standard slice control set').
    link($StandardSliceDefaultPrivilegeRef) :- spec('standard slice default privileges set').
    name("delegate/$SubjectId/$SliceId") :- spec("delegate slice control by slice root to subject $SubjectId").
  }.

defpost publishSliceControlSets() :- [standardSliceControlSet(), standardSliceDefaultPrivilegeSet()].

defpost publishSlice(?ProjectOwnerRef, ?SubjectId) :- 
   ?ProjectRef                       := "?ProjectOwnerRef:projectGuid",
   ?StandardSliceControlRef          := computeIdFromName($Self, 'object/standardSliceControl'),
   ?StandardSliceDefaultPrivilegeRef := computeIdFromName($Self, 'object/standardSliceDefaultPrivilege'),
   //?SliceId := scid(), 
   ?SliceId                          := "$Self:sliceGuid",
   [createSlice(?SubjectId, ?SliceId, ?ProjectRef, ?StandardSliceControlRef, ?StandardSliceDefaultPrivilegeRef)].

definit publishSliceControlSets(), publishSlice($2, $3).

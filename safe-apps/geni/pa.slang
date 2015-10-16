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

defcon memberSet() :- 
  {
    member(?User, ?Project) :-
      member(?User, ?Project, _).

    member(?User, ?Project, true) :-
      owner(?User, ?Project).
 
    member(?User, ?Project, ?Delegatable) :-
      ?Delegator: delegateMember(?User, ?Project, ?Delegatable),
      member(?Delegator, ?Project, true).
 
    memberPrivilege(?User, ?Project, instantiate, ?Delegatable) :- 
      member(?User, ?Project, ?Delegatable).

    memberPrivilege(?User, ?Project, ?Priv, ?Delegatable) :-
      ?Delegator: delegateMemberPrivilege(?User, ?Project, ?Priv, ?Delegatable),
      memberPrivilege(?Delegator, ?Project, ?Priv, true).
 
    name('object/standardProjectMembership') :- 
      spec('Object set for standard project membership policy issued by self').
  }.

defcon createProject(?SubjectId, ?Project, ?StandardProjectMembershipRef) :-
  {
    owner($SubjectId, $Project).
    project($Project).
    link($StandardProjectMembershipRef).
    name("capability/$Project/$SubjectId").
  }.

defpost publishMemberSet() :- [memberSet()].

defpost publishProject(?SubjectId) :- 
   ?StandardProjectMembershipRef := computeIdFromName($Self, 'object/standardProjectMembership'), 
   //?Project := scid(), // For testing, we hard code the project value
   ?ProjectRef := "?SubjectId:projectGuid",
   [createProject(?SubjectId, ?ProjectRef, ?StandardProjectMembershipRef)].

definit publishMemberSet(), publishProject($2).

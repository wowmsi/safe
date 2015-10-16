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

defcon makeMyIdSet(?CN) :-
  spec('Construct identity set for self'),
  {
    principal($SelfKey) :- spec("identity set for $CN").
  }.

defcon trustStructure(?CN) :-
  spec('Local policy for trust coordinators'),
  {
    //spec("Local policy for trust coordinators as defined by $CN").
    identityProvider(?IdP) :- geniRoot(?Geni), ?Geni: identityProvider(?IdP).
    sliceAuthority(?SA) :- geniRoot(?Geni), ?Geni: sliceAuthority(?SA).
    projectAuthority(?PA) :- geniRoot(?Geni), ?Geni: projectAuthority(?PA).
    geniProxy(?GeniProxy) :- geniRoot(?Geni), ?Geni: geniProxy(?GeniProxy).
    aggregate(?Agg) :- geniRoot(?Geni), ?Geni: aggregate(?Agg).
    gmoc(?GeniMOC) :- geniRoot(?Geni), ?Geni: gmoc(?GeniMOC).
    name('subject/trust-structure').
  }.

defcon registeredUserPolicy(?CN) :-
  spec('Local policy for validating GENI users and PIs'),
  {
    //spec("Local policy for validating GENI users and PIs as defined by $CN").
    geniUser(?User) :- identityProvider(?IdP), ?IdP: geniUser(?User).
    geniPI(?User) :- identityProvider(?IdP), ?IdP: geniPI(?User).
    name('subject/registered-user').
  }.

defpost bootstrap(?CN) :- [makeMyIdSet(?CN), trustStructure(?CN), registeredUserPolicy(?CN)].

definit bootstrap($2).

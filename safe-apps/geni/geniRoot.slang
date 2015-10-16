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

defcon endorseIdP(?IdP) :-
  spec('endorse an identity provider'),
  {
    identityProvider($IdP) :- spec("geni endorses an identity provider $IdP").
    name("endorse/idp/$IdP").
  }.

defcon endorsePA(?PA) :-
  spec('endorse a project authority'),
  {
    projectAuthority($PA) :- spec("geni endorses a project authority $PA").
    name("endorse/pa/$PA").
  }.

defcon endorseSA(?SA) :-
  spec('endorse a slice authority'),
  {
    sliceAuthority($SA) :- spec("geni endorses a slice authority $SA").
    name("endorse/sa/$SA").
  }.

defcon endorseCP(?CP) :-
  spec('endorse an aggregate'),
  {
    aggregate($CP) :- spec("geni endorses an aggregate $CP").
    name("endorse/cp/$CP").
  }.

defcon endorseAll(?IdP, ?PA, ?SA, ?CP) :-
 spec('link to all endorse cooridinators'),
 ?IdPRef := computeIdFromName($Self, "endorse/idp/?IdP"),
 ?PARef  := computeIdFromName($Self, "endorse/pa/?PA"),
 ?SARef  := computeIdFromName($Self, "endorse/sa/?SA"),
 ?CPRef  := computeIdFromName($Self, "endorse/cp/?CP"),
 {
   link($IdPRef) :- spec('IdP endorsement').
   link($PARef) :- spec('PA endorsement').
   link($SARef) :- spec('SA endorsement').
   link($CPRef) :- spec('CP endorsement').
   name(endorseAll).
 }.

defpost endorseCoordinators(?IdP, ?PA, ?SA, ?CP) :- 
  [endorseIdP(?IdP), endorsePA(?PA), endorseSA(?SA), endorseCP(?CP), endorseAll(?IdP, ?PA, ?SA, ?CP)].

definit endorseCoordinators($2, $3, $4, $5).

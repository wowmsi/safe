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



defenv Selfie() :-
  spec('Mint a new key pair using RSA algorithm of size 4096 bits'),
  principal($1).

defenv Two :- hello, 2.

defcon makeMyKeySet() :-
  spec('Construct key set for self'),
  ?Two := 4, // Q: What is Self is redefined here? and published under Selfie's Self?
  {
    principal($SelfKey). // should throw an error if principal predicate is not present
    test($SelfKey).
    number($Two).
    hello(?X) :- check(?X).
  }.

defcon makeFunSet() :-
  spec('Construct key set for self'),
  ?Two := 4, // Q: What is Self is redefined here? and published under Selfie's Self?
  funSet{
    number($Two).
    hello(?X) :- check(?X).
    10.
    hello(world).
    pred().
  }.

defcon makeFunMergeSet() :-
  spec('Construct key set for self'),
  ?Y := makeFunSet(),
  funSet{
    number($Two).
    hello(?X) :- check(?X).
    simple(World).
    parent(a, b).
    parent(b, c).
    ancestor(?X, ?Y) :- parent(?X, ?Y).
    ancestor(?X, ?Y) :- parent(?X, ?Z), ancestor(?Z, ?Y).
  }.

defpost postASet() :- [makeFunMergeSet()].

defetch fetchASet() :- [computeIdFromName($Self, funSet)].

defguard guardTest() :- 
  ?X := fetchASet(),
  {
    $Self: importAll($X).
    //$Self: number(?N)?
    //simple(?X)?
    ancestor(?X, ?Y)?
  }.

//definit makeMyKeySet().
//definit makeFunSet(), makeFunMergeSet().
//definit makeFunSet(), makeFunMergeSet().
//definit makeFunSet(), postASet(), guardTest().
//definit makeFunSet(), postASet().
definit guardTest().


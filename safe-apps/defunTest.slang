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




defenv Z :- 6.

defun funTest(?X, ?Y) :-
  ?Y := 2,
  `
     println("hi world")
     $X + $Y.toInt * $Z.toInt
  `.

defun funTest(?X, ?Y) :-
  ?Y := 4,
  false,
  `
     $X.toInt * $Y.toInt * $Z.toInt
  `.

assert defun funTest(?X, ?Y) if
  ?Y is 4 and
  true and // ;
  `
     $X.toInt * $Y.toInt * $Z.toInt
  `
  end

//funTest(?X, ?Y) :- ?Y := 4, -(?X, ?Y); ?Y := 10, -(?Y, ?X). // unsound rule


defun funTest(?X, ?Y) :-
  ?Z := 2,
  ?Y := 4,
  `
     $X.toInt * $Y.toInt * $Z.toInt
  `.

defun doubleFunTest(?X, ?Y) :-
  ?Z := funTest(?X, ?Y), // can diverge here due to multiple funTest definitions
  ?Y := 2,
  `
     $X.toInt * $Y.toInt * $Z.toInt
  `.

defun nestedFunTest(funTest(4, 6), ?Y) :-
  ?Y := 2,
  `
     $Z.toInt * $Y.toInt
  `.

//definit funTest(8, ?A).
definit doubleFunTest(8, ?A).
//definit nestedFunTest(?A, ?B).

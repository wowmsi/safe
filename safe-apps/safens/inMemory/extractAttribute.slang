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

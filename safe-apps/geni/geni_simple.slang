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
  principal($1),
end

defenv GeniRoot :- $2.

defetch fetchAllLinks() :-
  ?TrustStructure  := computeIdFromName($Self, 'subject/trust-structure'),
  ?GeniEndorsement := computeIdFromName($GeniRoot, 'endorseAll'),
  [?TrustStructure, ?GeniEndorsement],
end

defguard authorize() :-
  spec('authorize the subject to access the slice on given project'),
  ?CPPolicySet    := computeIdFromName($Self, 'object/aggregate-policy-slice-operations'),
  ?FetchedSets    := fetchAllLinks(),
  println('Printing fetchedSet links'),
  println(?FetchedSets),
  {
    importAll($FetchedSets).

    geniRoot($GeniRoot).

    sliceAuthority(?X)?
    hello(world).
    hello(?X)?
  },
end
 
definit authorize().

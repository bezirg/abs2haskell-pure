{-# LANGUAGE ImplicitParams #-}

module Main where

import ABS
import Debug.Trace

-- Attributes ids' trick
attrs@(actors:rounds                  -- parameters of the program
 :afirst:anext -- holding actors
 :iD:iDnext         -- each actor has an id (int) because we want to use arithmetic on it
 :rcurrent:rnext     -- each actor hold the current round number (int) and the next (we do arithmetic on it)
 :f1                 -- dummy future for async calls
 :vlast              -- a dummy object that holds the value 1
 :[]) = [1..10]

{-
main := 
   first := new;
   f1 := first ! init_first(n);
   await f1;                    -- wait for the init of all the actors to finish
   f2 := first ! go(r);
-}

main_ :: Method
main_ [] this wb k = 
  -- Parameters of the threadring program
   assign actors (Param 10) $ 
   assign rounds (Param 100) $ 
   assign afirst New $ 
   assign f1 (Async afirst init_first [actors]) $ 
   await f1 $ 
   trace "init finished" assign f1 (Async afirst go [rounds]) k

{-
init_first(i) :=
   id := i;
   first := this;
   next := new;
   f1 = next ! init_rest(n-1,first);
   await f1;                    -- wait for the init of the rest actors to finish
-}

init_first :: Method
init_first [i] this wb k = 
    assign iD (Param i) $ 
    assign iDnext (Param (i-1)) $ -- using int arithmetic on params is ok
    assign afirst (Param this) $ 
    assign anext New $ 
    assign f1 (Async anext init_rest [iDnext,afirst]) $ 
    await f1 $ 
    return_ iD wb k -- dummy
{-
init_rest(i,first_) :=
   this.id := n;
   first := first;
   if (not (id == 1)) {
     next := new;
     f = next ! init_rest(n-1,first);
     await f; -- wait for the init of the rest actors to finish
-}

init_rest :: Method
init_rest [i,first_] this wb k = 
    assign iD (Param i) $ 
    assign iDnext (Param (i-1)) $ -- using int arithmetic on params is ok
    assign afirst (Param first_) $ 
    assign vlast (Param 1) $      -- turning a pure value to an attribute to use in the next BEq
    if_ (BNeg (iD `BEq` vlast)) 
           (\ k' -> assign anext New $ 
                   assign f1 (Async anext init_rest [iDnext,afirst]) $ 
                   await f1 k')
           skip $ 
    return_ iD wb k -- dummy

           
{-
go(r) :=
   if (id == 1) {
        if (r == 1) {
                 skip;   // done   , should be reached by last actor
        }
        else {
           first ! go(r-1);
        }
   }
   else {
      next ! go(r);
   }
-}

go :: Method
go [round_] this wb k = 
    assign rcurrent (Param round_) $ 
    if_ (iD `BEq` vlast)
       (if_ (rcurrent `BEq` vlast) 
           skip  -- done
           (\ k' -> assign rnext (Param (round_ -1)) $ -- placeholder
                   assign f1 (Async afirst go [rnext]) k'))
       (assign f1 (Async anext go [rcurrent])) $ 
    return_ iD wb k              -- dummy


main :: IO ()
main = printHeap =<< run 10000 main_ (length attrs + 1)


{- new output

init finished
Real steps:	0
Total steps:	4765
Object Heap with array-size:1280{
(20,([(3,2),(5,1),(6,0),(7,1),(8,1),(9,1012),(10,1)],fromList []))(18,([(3,2),(4,20),(5,2),(6,1),(7,1),(9,1021),(10,1)],fromList []))(16,([(3,2),(4,18),(5,3),(6,2),(7,1),(9,1020),(10,1)],fromList []))(14,([(3,2),(4,16),(5,4),(6,3),(7,1),(9,1019),(10,1)],fromList []))(12,([(3,2),(4,14),(5,5),(6,4),(7,1),(9,1018),(10,1)],fromList []))(10,([(3,2),(4,12),(5,6),(6,5),(7,1),(9,1017),(10,1)],fromList []))(8,([(3,2),(4,10),(5,7),(6,6),(7,1),(9,1016),(10,1)],fromList []))(6,([(3,2),(4,8),(5,8),(6,7),(7,1),(9,1015),(10,1)],fromList []))(4,([(3,2),(4,6),(5,9),(6,8),(7,1),(9,1014),(10,1)],fromList []))(2,([(3,2),(4,4),(5,10),(6,9),(7,1),(9,1013)],fromList []))(0,([(0,-123),(1,10),(2,100),(3,2),(9,22)],fromList []))
}
Future Heap with array-size:1280{
(1021,Right 1)(1020,Right 2)(1019,Right 3)(1018,Right 4)(1017,Right 5)(1016,Right 6)(1015,Right 7)(1014,Right 8)(1013,Right 9)(1012,Right 10)(1011,Right 1)(1010,Right 2)(1009,Right 3)(1008,Right 4)(1007,Right 5)(1006,Right 6)(1005,Right 7)(1004,Right 8)(1003,Right 9)(1002,Right 10)(1001,Right 1)(1000,Right 2)(999,Right 3)(998,Right 4)(997,Right 5)(996,Right 6)(995,Right 7)(994,Right 8)(993,Right 9)(992,Right 10)(991,Right 1)(990,Right 2)(989,Right 3)(988,Right 4)(987,Right 5)(986,Right 6)(985,Right 7)(984,Right 8)(983,Right 9)(982,Right 10)(981,Right 1)(980,Right 2)(979,Right 3)(978,Right 4)(977,Right 5)(976,Right 6)(975,Right 7)(974,Right 8)(973,Right 9)(972,Right 10)(971,Right 1)(970,Right 2)(969,Right 3)(968,Right 4)(967,Right 5)(966,Right 6)(965,Right 7)(964,Right 8)(963,Right 9)(962,Right 10)(961,Right 1)(960,Right 2)(959,Right 3)(958,Right 4)(957,Right 5)(956,Right 6)(955,Right 7)(954,Right 8)(953,Right 9)(952,Right 10)(951,Right 1)(950,Right 2)(949,Right 3)(948,Right 4)(947,Right 5)(946,Right 6)(945,Right 7)(944,Right 8)(943,Right 9)(942,Right 10)(941,Right 1)(940,Right 2)(939,Right 3)(938,Right 4)(937,Right 5)(936,Right 6)(935,Right 7)(934,Right 8)(933,Right 9)(932,Right 10)(931,Right 1)(930,Right 2)(929,Right 3)(928,Right 4)(927,Right 5)(926,Right 6)(925,Right 7)(924,Right 8)(923,Right 9)(922,Right 10)(921,Right 1)(920,Right 2)(919,Right 3)(918,Right 4)(917,Right 5)(916,Right 6)(915,Right 7)(914,Right 8)(913,Right 9)(912,Right 10)(911,Right 1)(910,Right 2)(909,Right 3)(908,Right 4)(907,Right 5)(906,Right 6)(905,Right 7)(904,Right 8)(903,Right 9)(902,Right 10)(901,Right 1)(900,Right 2)(899,Right 3)(898,Right 4)(897,Right 5)(896,Right 6)(895,Right 7)(894,Right 8)(893,Right 9)(892,Right 10)(891,Right 1)(890,Right 2)(889,Right 3)(888,Right 4)(887,Right 5)(886,Right 6)(885,Right 7)(884,Right 8)(883,Right 9)(882,Right 10)(881,Right 1)(880,Right 2)(879,Right 3)(878,Right 4)(877,Right 5)(876,Right 6)(875,Right 7)(874,Right 8)(873,Right 9)(872,Right 10)(871,Right 1)(870,Right 2)(869,Right 3)(868,Right 4)(867,Right 5)(866,Right 6)(865,Right 7)(864,Right 8)(863,Right 9)(862,Right 10)(861,Right 1)(860,Right 2)(859,Right 3)(858,Right 4)(857,Right 5)(856,Right 6)(855,Right 7)(854,Right 8)(853,Right 9)(852,Right 10)(851,Right 1)(850,Right 2)(849,Right 3)(848,Right 4)(847,Right 5)(846,Right 6)(845,Right 7)(844,Right 8)(843,Right 9)(842,Right 10)(841,Right 1)(840,Right 2)(839,Right 3)(838,Right 4)(837,Right 5)(836,Right 6)(835,Right 7)(834,Right 8)(833,Right 9)(832,Right 10)(831,Right 1)(830,Right 2)(829,Right 3)(828,Right 4)(827,Right 5)(826,Right 6)(825,Right 7)(824,Right 8)(823,Right 9)(822,Right 10)(821,Right 1)(820,Right 2)(819,Right 3)(818,Right 4)(817,Right 5)(816,Right 6)(815,Right 7)(814,Right 8)(813,Right 9)(812,Right 10)(811,Right 1)(810,Right 2)(809,Right 3)(808,Right 4)(807,Right 5)(806,Right 6)(805,Right 7)(804,Right 8)(803,Right 9)(802,Right 10)(801,Right 1)(800,Right 2)(799,Right 3)(798,Right 4)(797,Right 5)(796,Right 6)(795,Right 7)(794,Right 8)(793,Right 9)(792,Right 10)(791,Right 1)(790,Right 2)(789,Right 3)(788,Right 4)(787,Right 5)(786,Right 6)(785,Right 7)(784,Right 8)(783,Right 9)(782,Right 10)(781,Right 1)(780,Right 2)(779,Right 3)(778,Right 4)(777,Right 5)(776,Right 6)(775,Right 7)(774,Right 8)(773,Right 9)(772,Right 10)(771,Right 1)(770,Right 2)(769,Right 3)(768,Right 4)(767,Right 5)(766,Right 6)(765,Right 7)(764,Right 8)(763,Right 9)(762,Right 10)(761,Right 1)(760,Right 2)(759,Right 3)(758,Right 4)(757,Right 5)(756,Right 6)(755,Right 7)(754,Right 8)(753,Right 9)(752,Right 10)(751,Right 1)(750,Right 2)(749,Right 3)(748,Right 4)(747,Right 5)(746,Right 6)(745,Right 7)(744,Right 8)(743,Right 9)(742,Right 10)(741,Right 1)(740,Right 2)(739,Right 3)(738,Right 4)(737,Right 5)(736,Right 6)(735,Right 7)(734,Right 8)(733,Right 9)(732,Right 10)(731,Right 1)(730,Right 2)(729,Right 3)(728,Right 4)(727,Right 5)(726,Right 6)(725,Right 7)(724,Right 8)(723,Right 9)(722,Right 10)(721,Right 1)(720,Right 2)(719,Right 3)(718,Right 4)(717,Right 5)(716,Right 6)(715,Right 7)(714,Right 8)(713,Right 9)(712,Right 10)(711,Right 1)(710,Right 2)(709,Right 3)(708,Right 4)(707,Right 5)(706,Right 6)(705,Right 7)(704,Right 8)(703,Right 9)(702,Right 10)(701,Right 1)(700,Right 2)(699,Right 3)(698,Right 4)(697,Right 5)(696,Right 6)(695,Right 7)(694,Right 8)(693,Right 9)(692,Right 10)(691,Right 1)(690,Right 2)(689,Right 3)(688,Right 4)(687,Right 5)(686,Right 6)(685,Right 7)(684,Right 8)(683,Right 9)(682,Right 10)(681,Right 1)(680,Right 2)(679,Right 3)(678,Right 4)(677,Right 5)(676,Right 6)(675,Right 7)(674,Right 8)(673,Right 9)(672,Right 10)(671,Right 1)(670,Right 2)(669,Right 3)(668,Right 4)(667,Right 5)(666,Right 6)(665,Right 7)(664,Right 8)(663,Right 9)(662,Right 10)(661,Right 1)(660,Right 2)(659,Right 3)(658,Right 4)(657,Right 5)(656,Right 6)(655,Right 7)(654,Right 8)(653,Right 9)(652,Right 10)(651,Right 1)(650,Right 2)(649,Right 3)(648,Right 4)(647,Right 5)(646,Right 6)(645,Right 7)(644,Right 8)(643,Right 9)(642,Right 10)(641,Right 1)(640,Right 2)(639,Right 3)(638,Right 4)(637,Right 5)(636,Right 6)(635,Right 7)(634,Right 8)(633,Right 9)(632,Right 10)(631,Right 1)(630,Right 2)(629,Right 3)(628,Right 4)(627,Right 5)(626,Right 6)(625,Right 7)(624,Right 8)(623,Right 9)(622,Right 10)(621,Right 1)(620,Right 2)(619,Right 3)(618,Right 4)(617,Right 5)(616,Right 6)(615,Right 7)(614,Right 8)(613,Right 9)(612,Right 10)(611,Right 1)(610,Right 2)(609,Right 3)(608,Right 4)(607,Right 5)(606,Right 6)(605,Right 7)(604,Right 8)(603,Right 9)(602,Right 10)(601,Right 1)(600,Right 2)(599,Right 3)(598,Right 4)(597,Right 5)(596,Right 6)(595,Right 7)(594,Right 8)(593,Right 9)(592,Right 10)(591,Right 1)(590,Right 2)(589,Right 3)(588,Right 4)(587,Right 5)(586,Right 6)(585,Right 7)(584,Right 8)(583,Right 9)(582,Right 10)(581,Right 1)(580,Right 2)(579,Right 3)(578,Right 4)(577,Right 5)(576,Right 6)(575,Right 7)(574,Right 8)(573,Right 9)(572,Right 10)(571,Right 1)(570,Right 2)(569,Right 3)(568,Right 4)(567,Right 5)(566,Right 6)(565,Right 7)(564,Right 8)(563,Right 9)(562,Right 10)(561,Right 1)(560,Right 2)(559,Right 3)(558,Right 4)(557,Right 5)(556,Right 6)(555,Right 7)(554,Right 8)(553,Right 9)(552,Right 10)(551,Right 1)(550,Right 2)(549,Right 3)(548,Right 4)(547,Right 5)(546,Right 6)(545,Right 7)(544,Right 8)(543,Right 9)(542,Right 10)(541,Right 1)(540,Right 2)(539,Right 3)(538,Right 4)(537,Right 5)(536,Right 6)(535,Right 7)(534,Right 8)(533,Right 9)(532,Right 10)(531,Right 1)(530,Right 2)(529,Right 3)(528,Right 4)(527,Right 5)(526,Right 6)(525,Right 7)(524,Right 8)(523,Right 9)(522,Right 10)(521,Right 1)(520,Right 2)(519,Right 3)(518,Right 4)(517,Right 5)(516,Right 6)(515,Right 7)(514,Right 8)(513,Right 9)(512,Right 10)(511,Right 1)(510,Right 2)(509,Right 3)(508,Right 4)(507,Right 5)(506,Right 6)(505,Right 7)(504,Right 8)(503,Right 9)(502,Right 10)(501,Right 1)(500,Right 2)(499,Right 3)(498,Right 4)(497,Right 5)(496,Right 6)(495,Right 7)(494,Right 8)(493,Right 9)(492,Right 10)(491,Right 1)(490,Right 2)(489,Right 3)(488,Right 4)(487,Right 5)(486,Right 6)(485,Right 7)(484,Right 8)(483,Right 9)(482,Right 10)(481,Right 1)(480,Right 2)(479,Right 3)(478,Right 4)(477,Right 5)(476,Right 6)(475,Right 7)(474,Right 8)(473,Right 9)(472,Right 10)(471,Right 1)(470,Right 2)(469,Right 3)(468,Right 4)(467,Right 5)(466,Right 6)(465,Right 7)(464,Right 8)(463,Right 9)(462,Right 10)(461,Right 1)(460,Right 2)(459,Right 3)(458,Right 4)(457,Right 5)(456,Right 6)(455,Right 7)(454,Right 8)(453,Right 9)(452,Right 10)(451,Right 1)(450,Right 2)(449,Right 3)(448,Right 4)(447,Right 5)(446,Right 6)(445,Right 7)(444,Right 8)(443,Right 9)(442,Right 10)(441,Right 1)(440,Right 2)(439,Right 3)(438,Right 4)(437,Right 5)(436,Right 6)(435,Right 7)(434,Right 8)(433,Right 9)(432,Right 10)(431,Right 1)(430,Right 2)(429,Right 3)(428,Right 4)(427,Right 5)(426,Right 6)(425,Right 7)(424,Right 8)(423,Right 9)(422,Right 10)(421,Right 1)(420,Right 2)(419,Right 3)(418,Right 4)(417,Right 5)(416,Right 6)(415,Right 7)(414,Right 8)(413,Right 9)(412,Right 10)(411,Right 1)(410,Right 2)(409,Right 3)(408,Right 4)(407,Right 5)(406,Right 6)(405,Right 7)(404,Right 8)(403,Right 9)(402,Right 10)(401,Right 1)(400,Right 2)(399,Right 3)(398,Right 4)(397,Right 5)(396,Right 6)(395,Right 7)(394,Right 8)(393,Right 9)(392,Right 10)(391,Right 1)(390,Right 2)(389,Right 3)(388,Right 4)(387,Right 5)(386,Right 6)(385,Right 7)(384,Right 8)(383,Right 9)(382,Right 10)(381,Right 1)(380,Right 2)(379,Right 3)(378,Right 4)(377,Right 5)(376,Right 6)(375,Right 7)(374,Right 8)(373,Right 9)(372,Right 10)(371,Right 1)(370,Right 2)(369,Right 3)(368,Right 4)(367,Right 5)(366,Right 6)(365,Right 7)(364,Right 8)(363,Right 9)(362,Right 10)(361,Right 1)(360,Right 2)(359,Right 3)(358,Right 4)(357,Right 5)(356,Right 6)(355,Right 7)(354,Right 8)(353,Right 9)(352,Right 10)(351,Right 1)(350,Right 2)(349,Right 3)(348,Right 4)(347,Right 5)(346,Right 6)(345,Right 7)(344,Right 8)(343,Right 9)(342,Right 10)(341,Right 1)(340,Right 2)(339,Right 3)(338,Right 4)(337,Right 5)(336,Right 6)(335,Right 7)(334,Right 8)(333,Right 9)(332,Right 10)(331,Right 1)(330,Right 2)(329,Right 3)(328,Right 4)(327,Right 5)(326,Right 6)(325,Right 7)(324,Right 8)(323,Right 9)(322,Right 10)(321,Right 1)(320,Right 2)(319,Right 3)(318,Right 4)(317,Right 5)(316,Right 6)(315,Right 7)(314,Right 8)(313,Right 9)(312,Right 10)(311,Right 1)(310,Right 2)(309,Right 3)(308,Right 4)(307,Right 5)(306,Right 6)(305,Right 7)(304,Right 8)(303,Right 9)(302,Right 10)(301,Right 1)(300,Right 2)(299,Right 3)(298,Right 4)(297,Right 5)(296,Right 6)(295,Right 7)(294,Right 8)(293,Right 9)(292,Right 10)(291,Right 1)(290,Right 2)(289,Right 3)(288,Right 4)(287,Right 5)(286,Right 6)(285,Right 7)(284,Right 8)(283,Right 9)(282,Right 10)(281,Right 1)(280,Right 2)(279,Right 3)(278,Right 4)(277,Right 5)(276,Right 6)(275,Right 7)(274,Right 8)(273,Right 9)(272,Right 10)(271,Right 1)(270,Right 2)(269,Right 3)(268,Right 4)(267,Right 5)(266,Right 6)(265,Right 7)(264,Right 8)(263,Right 9)(262,Right 10)(261,Right 1)(260,Right 2)(259,Right 3)(258,Right 4)(257,Right 5)(256,Right 6)(255,Right 7)(254,Right 8)(253,Right 9)(252,Right 10)(251,Right 1)(250,Right 2)(249,Right 3)(248,Right 4)(247,Right 5)(246,Right 6)(245,Right 7)(244,Right 8)(243,Right 9)(242,Right 10)(241,Right 1)(240,Right 2)(239,Right 3)(238,Right 4)(237,Right 5)(236,Right 6)(235,Right 7)(234,Right 8)(233,Right 9)(232,Right 10)(231,Right 1)(230,Right 2)(229,Right 3)(228,Right 4)(227,Right 5)(226,Right 6)(225,Right 7)(224,Right 8)(223,Right 9)(222,Right 10)(221,Right 1)(220,Right 2)(219,Right 3)(218,Right 4)(217,Right 5)(216,Right 6)(215,Right 7)(214,Right 8)(213,Right 9)(212,Right 10)(211,Right 1)(210,Right 2)(209,Right 3)(208,Right 4)(207,Right 5)(206,Right 6)(205,Right 7)(204,Right 8)(203,Right 9)(202,Right 10)(201,Right 1)(200,Right 2)(199,Right 3)(198,Right 4)(197,Right 5)(196,Right 6)(195,Right 7)(194,Right 8)(193,Right 9)(192,Right 10)(191,Right 1)(190,Right 2)(189,Right 3)(188,Right 4)(187,Right 5)(186,Right 6)(185,Right 7)(184,Right 8)(183,Right 9)(182,Right 10)(181,Right 1)(180,Right 2)(179,Right 3)(178,Right 4)(177,Right 5)(176,Right 6)(175,Right 7)(174,Right 8)(173,Right 9)(172,Right 10)(171,Right 1)(170,Right 2)(169,Right 3)(168,Right 4)(167,Right 5)(166,Right 6)(165,Right 7)(164,Right 8)(163,Right 9)(162,Right 10)(161,Right 1)(160,Right 2)(159,Right 3)(158,Right 4)(157,Right 5)(156,Right 6)(155,Right 7)(154,Right 8)(153,Right 9)(152,Right 10)(151,Right 1)(150,Right 2)(149,Right 3)(148,Right 4)(147,Right 5)(146,Right 6)(145,Right 7)(144,Right 8)(143,Right 9)(142,Right 10)(141,Right 1)(140,Right 2)(139,Right 3)(138,Right 4)(137,Right 5)(136,Right 6)(135,Right 7)(134,Right 8)(133,Right 9)(132,Right 10)(131,Right 1)(130,Right 2)(129,Right 3)(128,Right 4)(127,Right 5)(126,Right 6)(125,Right 7)(124,Right 8)(123,Right 9)(122,Right 10)(121,Right 1)(120,Right 2)(119,Right 3)(118,Right 4)(117,Right 5)(116,Right 6)(115,Right 7)(114,Right 8)(113,Right 9)(112,Right 10)(111,Right 1)(110,Right 2)(109,Right 3)(108,Right 4)(107,Right 5)(106,Right 6)(105,Right 7)(104,Right 8)(103,Right 9)(102,Right 10)(101,Right 1)(100,Right 2)(99,Right 3)(98,Right 4)(97,Right 5)(96,Right 6)(95,Right 7)(94,Right 8)(93,Right 9)(92,Right 10)(91,Right 1)(90,Right 2)(89,Right 3)(88,Right 4)(87,Right 5)(86,Right 6)(85,Right 7)(84,Right 8)(83,Right 9)(82,Right 10)(81,Right 1)(80,Right 2)(79,Right 3)(78,Right 4)(77,Right 5)(76,Right 6)(75,Right 7)(74,Right 8)(73,Right 9)(72,Right 10)(71,Right 1)(70,Right 2)(69,Right 3)(68,Right 4)(67,Right 5)(66,Right 6)(65,Right 7)(64,Right 8)(63,Right 9)(62,Right 10)(61,Right 1)(60,Right 2)(59,Right 3)(58,Right 4)(57,Right 5)(56,Right 6)(55,Right 7)(54,Right 8)(53,Right 9)(52,Right 10)(51,Right 1)(50,Right 2)(49,Right 3)(48,Right 4)(47,Right 5)(46,Right 6)(45,Right 7)(44,Right 8)(43,Right 9)(42,Right 10)(41,Right 1)(40,Right 2)(39,Right 3)(38,Right 4)(37,Right 5)(36,Right 6)(35,Right 7)(34,Right 8)(33,Right 9)(32,Right 10)(31,Right 1)(30,Right 2)(29,Right 3)(28,Right 4)(27,Right 5)(26,Right 6)(25,Right 7)(24,Right 8)(23,Right 9)(22,Right 10)(21,Right 1)(19,Right 2)(17,Right 3)(15,Right 4)(13,Right 5)(11,Right 6)(9,Right 7)(7,Right 8)(5,Right 9)(3,Right 10)(1,Right (-123))
}
    Counter: 1022
-}

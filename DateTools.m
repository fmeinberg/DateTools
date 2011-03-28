BeginPackage["DateTools`"]

DatePlusList::usage="";
DateRange::usage="";
cal::usage="";

Begin["Private`"];

DatePlusList[start_, {lookahead_, d_}]:=
	NestList[DatePlus[#, {1, d}]&, DateList@start, lookahead]

DateRange[start_, end_, d_:{1, "Day"}]:=
	Block[{abs = AbsoluteTime@end, dd = If[MatchQ[d, _String], {1, d}, d], foo},
		foo = Switch[Sign[dd[[1]]], 1, LessEqual, -1, GreaterEqual];
		NestWhileList[DatePlus[#, dd]&, start,
			foo[AbsoluteTime@#, abs]&, 1, Infinity, -1]
	]

(* This calls the Unix utility cal from within Mathematica *)

cal[spec_] :=
 With[{file = ToFileName[$TemporaryDirectory, "cal.txt"]},
  Run["cal " <> spec <> " > " <> file]; Import[file, "String"]]

End[];
EndPackage[];

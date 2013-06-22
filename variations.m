(* ::Package:: *)

(*:Title: Variations *)

(* :Author: Riccardo V. Vincelli, Universita' di Milano-Bicocca *)
(* :Mathematica Version: 8.0 *) 
(* :Package Version: 1.0 *) 
(* :Context: Variations`*)
(* :Summary: See README *)          
(* :History: Wanted to put at test the new forward transforms *)
(* Botherings? Mail: Riccardo V. Vincelli - r.vincelli@campus.unimib.it *)
(* License: See README *)

		(**Variations**)


BeginPackage["Variations`"]

Print[Date[][[1]],"-",Date[][[2]],"-",Date[][[3]]]
Print["package by: Riccardo aka riQui Vincelli"]
Print["Type ?Variations`* to visualize package functions"]
Print["Now loading ..."]

Unprotect[
Ball,
Bent,
Cosine,
Cilindro,
Disc,
Exponential,
Fisheye,
Halfbadge,
Handkerchief,
Helix,
Horsehoe,
Linear,
Randomized,
Shatter,
Sinusoidal,
Slice,
Spherical,
Stone,
Swirl,
Tangent
]

(*Ball*)
Ball::usage = "ParametricPlot[{Sin[Pi y]-Cos[Pi x],Sin[Pi x]-Cos[Pi y]},{x,-1,1},{y,-1,1}]"

(*Bent*)
Bent::usage = "f[p_]:=\[IndentingNewLine]If[p[[1]]\[GreaterEqual]0&&p[[2]]\[GreaterEqual]0,p,If[p[[1]]<0&&p[[2]]\[GreaterEqual]0,{2p[[1]],p[[2]]},If[p[[1]]\[GreaterEqual]0&&p[[2]]<0,{p[[1]],p[[2]]/2},{2p[[1]],p[[2]]/2}]]]
ParametricPlot[{f[{x,y}][[1]],f[{x,y}][[2]]},{x,-1,1},{y,-1,1}]"

(*Cosine*)
Cosine::usage = "ParametricPlot[{Cos[Pi x]Cosh[y],-Sin[Pi x]Sinh[y]},{x,-1,1},{y,-1,1}]"

(*Cilindro*)
Cilindro::usage = "ParametricPlot[{Sin[x],y},{x,-1,1},{y,-1,1}]"

(*Disc*)
Disc::usage = "ParametricPlot[{(ArcTan[x/y]/Pi)Sin[Pi Sqrt[x^2+y^2]],(ArcTan[x/y]/Pi)Cos[Pi Sqrt[x^2+y^2]]},{x,-1,1},{y,-1,1}]"

(*Exponential*)
Exponential::usage = "ParametricPlot[{Exp[x-1]Cos[Pi y],Exp[x-1]Sin[Pi y]},{x,-1,1},{y,-1,1}]"

(*Fisheye*)
Fisheye::usage = "ParametricPlot[{2y/(Sqrt[x^2+y^2]+1),2x/(Sqrt[x^2+y^2]+1)},{x,-1,1},{y,-1,1}]"

(*Halfbadge*)
Halfbadge::usage = "ParametricPlot[{Re[(u+\[ImaginaryI] v\!\(\*SuperscriptBox[\()\), \(.5\)]\)],Im[(u+\[ImaginaryI] v\!\(\*SuperscriptBox[\()\), \(.5\)]\)]},{u,-2,2},{v,-2,2}]"

(*Handkerchief*)
Handkerchief::usage = "ParametricPlot[{Sqrt[x^2+y^2]Sin[ArcTan[x/y]+Sqrt[x^2+y^2]],Sqrt[x^2+y^2]Cos[ArcTan[x/y]-Sqrt[x^2+y^2]]},{x,-1,1},{y,-1,1}]"

(*Helix*)
Helix::usage = "ParametricPlot[{Sqrt[x^2+y^2](-2Pi+((2Pi+2Pi)/2)(Sin[Pi ArcTan[x/y]]+1))Cos[ArcTan[x/y]],Sqrt[x^2+y^2](-2Pi+((2Pi+2Pi)/2)(Sin[Pi ArcTan[x/y]]+1))Sin[ArcTan[x/y]]},{x,-1,1},{y,-1,1}]"

(*Horsehoe*)
Horsehoe::usage = "ParametricPlot[{(x-y)(x+y)/Sqrt[(x^2+y^2)],2x y/Sqrt[(x^2+y^2)]},{x,-1,1},{y,-1,1}]"

(*Linear*)
Linear::usage = "identity!"

(*Randomized*)
Randomized::usage = "ParametricPlot[{x+Random[]Sin[Tan[3y]],y+Random[]Sin[Tan[3x]]},{x,-1,1},{y,-1,1}]"

(*Shatter*)
Shatter::usage = "ParametricPlot[{x+Random[]Sin[y/Random[]^2],y+Random[]Sin[x/Random[]^2]},{x,-8Pi,8Pi},{y,-8Pi,8Pi}]"

(*Sinusoidal*)
Sinusoidal::usage = "ParametricPlot[{Sin[x],Sin[y]},{x,-1,1},{y,-1,1}]"

(*Slice*)
Slice::usage = "ParametricPlot[{v Cos[u],v Sin[u]},{u,0,Pi/3},{v,0,1}]"

(*Spherical*)
Spherical::usage = "
ParametricPlot[{x/(x^2+y^2),y/(x^2+y^2)},{x,0,1},{y,0,1}]
ParametricPlot[{x/(x^2+y^2),y/(x^2+y^2)},{x,-1,0},{y,0,1}]
ParametricPlot[{x/(x^2+y^2),y/(x^2+y^2)},{x,0,1},{y,-1,0}]
ParametricPlot[{x/(x^2+y^2),y/(x^2+y^2)},{x,-1,0},{y,-1,0}]
"

(*Stone*)
Stone::usage = "ParametricPlot[{-Sqrt[Sqrt[b^2+a^2]]Sin[ArcTan[b/a]/2],-Sqrt[Sqrt[b^2+a^2]]Cos[ArcTan[b/a]/2]},{a,-1,1},{b,-1,1}]"

(*Swirl*)
Swirl::usage = "ParametricPlot[{x Sin[x^2+y^2]-y Cos[x^2+y^2],x Cos[x^2+y^2]+y Sin[x^2+y^2]},{x,-1,1},{y,-1,1}]"

(*Tangent*)
Tangent::usage = "ParametricPlot[{Sin[x]/Cos[y], Tan[y]},{x,-1,1},{y,-1,1}]"


Begin["`Private`"]

Ball[img_]:=ImageCrop[ImageForwardTransformation[img,{Sin[Pi #[[2]]]-Cos[Pi #[[1]]],Sin[Pi #[[1]]]-Cos[Pi #[[2]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic]]

Bent[img_]:= Module[{f},
f[p_]:=If[p[[1]]>=0&&p[[2]]>=0,p,If[p[[1]]<0&&p[[2]]>=0,{2p[[1]],p[[2]]},If[p[[1]]>=0&&p[[2]]<0,{p[[1]],p[[2]]/2},{2p[[1]],p[[2]]/2}]]];
ImageCrop[ImageForwardTransformation[img,{f[{#[[1]],#[[2]]}][[1]],f[{#[[1]],#[[2]]}][[2]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic]]
]

Cosine[img_]:=ImageCrop[ImageForwardTransformation[img,{Cos[Pi #[[1]]]Cosh[#[[2]]],-Sin[Pi #[[1]]]Sinh[#[[2]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Full]]

Cilindro[img_]:=ImageCrop[ImageForwardTransformation[img,{Sin[#[[1]]],#[[2]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic]]

Disc[img_]:=ImageCrop[ImageForwardTransformation[img,{(ArcTan[#[[1]]/#[[2]]]/Pi)Sin[Pi Sqrt[#[[1]]^2+#[[2]]^2]],(ArcTan[#[[1]]/#[[2]]]/Pi)Cos[Pi Sqrt[#[[1]]^2+#[[2]]^2]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]

Exponential[img_]:=ImageCrop[ImageForwardTransformation[img,{Exp[#[[1]]-1]Cos[Pi #[[2]]],Exp[#[[1]]-1]Sin[Pi #[[2]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]

Fisheye[img_]:=ImageReflect[ImageRotate[ImageCrop[ImageForwardTransformation[img,{2#[[2]]/(Sqrt[#[[1]]^2+#[[2]]^2]+1),2#[[1]]/(Sqrt[#[[1]]^2+#[[2]]^2]+1)}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]], Left -> Right]

Halfbadge[img_]:=ImageCrop[ImageForwardTransformation[img,{Re[(#[[1]]+I #[[2]])^.5],Im[(#[[1]]+I #[[2]])^.5]}&,DataRange->{{-2,2},{-2,2}},PlotRange->Automatic,Background->White]]

Handkerchief[img_]:=ImageForwardTransformation[img,{Sqrt[#[[1]]^2+#[[2]]^2]Sin[ArcTan[#[[1]]/#[[2]]]+Sqrt[#[[1]]^2+#[[2]]^2]],Sqrt[#[[1]]^2+#[[2]]^2]Cos[ArcTan[#[[1]]/#[[2]]]-Sqrt[#[[1]]^2+#[[2]]^2]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]

Helix[img_]:=ImageCrop[ImageForwardTransformation[img,
{Sqrt[#[[1]]^2+#[[2]]^2](-2Pi+((2Pi+2Pi)/2)(Sin[Pi ArcTan[#[[1]]/#[[2]]]]+1))Cos[ArcTan[#[[1]]/#[[2]]]],Sqrt[#[[1]]^2+#[[2]]^2](-2Pi+((2Pi+2Pi)/2)(Sin[Pi ArcTan[#[[1]]/#[[2]]]]+1))Sin[ArcTan[#[[1]]/#[[2]]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]

Horsehoe[img_]:=ImageForwardTransformation[img,{(#[[1]]-#[[2]])(#[[1]]+#[[2]])/Sqrt[(#[[1]]^2+#[[2]]^2)],2#[[1]] #[[2]]/Sqrt[(#[[1]]^2+#[[2]]^2)]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]

Linear[img_]:=ImageCrop[ImageForwardTransformation[img,{#[[1]],#[[2]]}&]]

Randomized[img_]:=ImageCrop[ImageForwardTransformation[img,{#[[1]]+Random[]Sin[Tan[3#[[2]]]],#[[2]]+Random[]Sin[Tan[3#[[1]]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic]]

Shatter[img_]:=ImageCrop[ImageForwardTransformation[img,{#[[1]]+Random[]Sin[#[[2]]/Random[]^2],#[[2]]+Random[]Sin[#[[1]]/Random[]^2]}&,DataRange->{{-8Pi,8Pi},{-8Pi,8Pi}},PlotRange->Automatic]]

Sinusoidal[img_]:=ImageCrop[ImageForwardTransformation[img,{Sin[#[[1]]],Sin[#[[2]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic]]

Slice[img_]:=ImageCrop[ImageForwardTransformation[img,{#[[2]] Cos[#[[1]]],#[[2]]Sin[#[[1]]]}&,DataRange->{{0,Pi/3},{0,1}},PlotRange->Automatic,Background->White]]

Spherical[img_]:= ImageAssemble[{
	{
		ImageForwardTransformation[ImageResize[img,{400,300}],{#[[1]]/Sqrt[#[[1]]^2+#[[2]]^2]^2,#[[2]]/Sqrt[#[[1]]^2+#[[2]]^2]^2}&,DataRange->{{-1,0},{0,1}},PlotRange->Automatic,Background->White],
		ImageForwardTransformation[ImageResize[img,{400,300}],{#[[1]]/Sqrt[#[[1]]^2+#[[2]]^2]^2,#[[2]]/Sqrt[#[[1]]^2+#[[2]]^2]^2}&,DataRange->{{0,1},{0,1}},PlotRange->Automatic,Background->White]
	}, {
		ImageForwardTransformation[ImageResize[img,{400,300}],{#[[1]]/Sqrt[#[[1]]^2+#[[2]]^2]^2,#[[2]]/Sqrt[#[[1]]^2+#[[2]]^2]^2}&,DataRange->{{-1,0},{-1,0}},PlotRange->Automatic,Background->White],
		ImageForwardTransformation[ImageResize[img,{400,300}],{#[[1]]/Sqrt[#[[1]]^2+#[[2]]^2]^2,#[[2]]/Sqrt[#[[1]]^2+#[[2]]^2]^2}&,DataRange->{{0,1},{-1,0}},PlotRange->Automatic,Background->White]
	}
}]

Stone[img_]:=ImageCrop[ImageForwardTransformation[img,{Sqrt[Sqrt[#[[1]]^2+#[[2]]^2]]Cos[ArcTan[#[[1]]/#[[2]]]/2],Sqrt[Sqrt[#[[1]]^2+#[[2]]^2]]Sin[ArcTan[#[[1]]/#[[2]]]/2]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]

Swirl[img_]:=ImageCrop[ImageForwardTransformation[img,{#[[1]] Sin[#[[1]]^2+#[[2]]^2]-#[[2]] Cos[#[[1]]^2+#[[2]]^2],#[[1]]Cos[#[[1]]^2+#[[2]]^2]+#[[2]] Sin[#[[1]]^2+#[[2]]^2]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]

Tangent[img_]:=ImageCrop[ImageForwardTransformation[img,{Sin[#[[1]]]/Cos[#[[2]]],Tan[#[[2]]]}&,DataRange->{{-1,1},{-1,1}},PlotRange->Automatic,Background->White]]

End[ ]
Print["ready..."]
EndPackage[ ]


this.libSet = new Set ()
this.libs = []
this.addLib = function (lib, decl) { if (!this.libSet.has (lib +'.'+decl)) { this.libSet.add (lib +'.'+decl); this.libs.push ({lib:lib, decl:decl})} }
this.loadlibs = function (cb) { rt.linkLibs (this.libs, this, cb) }
this.serializedatoms = "AQAAAAAAAAAA"
this.gensym16 = function ($env,inputLineAtLevel_arg213) {
  rt.push (($decltemp$18) =>
           {const gensym19 = rt.mkCopy(rt.declassify);
            const gensym20 = rt.mkVal(rt.mkTuple([$decltemp$18, $env.inputLineAtLevel_arg112, inputLineAtLevel_arg213]));
            rt.tailcall (gensym19,gensym20);});
  rt.tailcall ($env.inputLineWithPini0,$env.inputLineAtLevel_arg112);
}
this.gensym16.deps = [];
this.gensym16.libdeps = [];
this.gensym16.serialized = "AAAAAAAAAAAIZ2Vuc3ltMTYAAAAAAAAAF2lucHV0TGluZUF0TGV2ZWxfYXJnMjEzAAAAAAAAAAAGAAAAAAAAAAwkZGVjbHRlbXAkMTgAAAAAAAAAAAABAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTABAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAACAAAAAAAAAAAIZ2Vuc3ltMTkGAAAAAAAAAApkZWNsYXNzaWZ5AAAAAAAAAAAIZ2Vuc3ltMjACAAAAAAAAAAMAAAAAAAAAAAwkZGVjbHRlbXAkMTgBAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAAAF2lucHV0TGluZUF0TGV2ZWxfYXJnMjEzAAAAAAAAAAAACGdlbnN5bTE5AAAAAAAAAAAIZ2Vuc3ltMjA=";
this.inputLineAtLevel11 = function ($env,inputLineAtLevel_arg112) {
  const $$$env0 = new rt.Env();
  $$$env0.inputLineAtLevel_arg112 = inputLineAtLevel_arg112;
  $$$env0.inputLineWithPini0 = $env.inputLineWithPini0;
  const gensym16 = rt.mkVal(new rt.Closure($$$env0, this, this.gensym16))
  $$$env0.gensym16 = gensym16;
  $$$env0.gensym16.selfpointer = true;
  rt.ret (gensym16);
}
this.inputLineAtLevel11.deps = ['gensym16'];
this.inputLineAtLevel11.libdeps = [];
this.inputLineAtLevel11.serialized = "AAAAAAAAAAASaW5wdXRMaW5lQXRMZXZlbDExAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAABAQAAAAAAAAACAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAAAF2lucHV0TGluZUF0TGV2ZWxfYXJnMTEyAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTABAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTAAAAAAAAAAAQAAAAAAAAAIZ2Vuc3ltMTYAAAAAAAAACGdlbnN5bTE2AQAAAAAAAAAACGdlbnN5bTE2";
this.inputLineWithPini0 = function ($env,inputLineWithPini_arg11) {
  rt.push (($decltemp$5) =>
           {rt.push (($decltemp$7) =>
                     {rt.push (($decltemp$9) =>
                               {rt.ret ($decltemp$7);});
                      const gensym3 = rt.mkCopy(rt.pinipop);
                      rt.tailcall (gensym3,$decltemp$5);});
            const gensym4 = rt.mkCopy(rt.inputLine);
            const gensym5 = rt.__unit;
            rt.tailcall (gensym4,gensym5);});
  const gensym6 = rt.mkCopy(rt.pinipush);
  rt.tailcall (gensym6,inputLineWithPini_arg11);
}
this.inputLineWithPini0.deps = [];
this.inputLineWithPini0.libdeps = [];
this.inputLineWithPini0.serialized = "AAAAAAAAAAASaW5wdXRMaW5lV2l0aFBpbmkwAAAAAAAAABdpbnB1dExpbmVXaXRoUGluaV9hcmcxMQAAAAAAAAAABgAAAAAAAAALJGRlY2x0ZW1wJDUAAAAAAAAAAQAAAAAAAAAAB2dlbnN5bTYGAAAAAAAAAAhwaW5pcHVzaAAAAAAAAAAAAAdnZW5zeW02AAAAAAAAAAAXaW5wdXRMaW5lV2l0aFBpbmlfYXJnMTEAAAAAAAAAAAYAAAAAAAAACyRkZWNsdGVtcCQ3AAAAAAAAAAIAAAAAAAAAAAdnZW5zeW00BgAAAAAAAAAJaW5wdXRMaW5lAAAAAAAAAAAHZ2Vuc3ltNQUDAAAAAAAAAAAAB2dlbnN5bTQAAAAAAAAAAAdnZW5zeW01AAAAAAAAAAAGAAAAAAAAAAskZGVjbHRlbXAkOQAAAAAAAAABAAAAAAAAAAAHZ2Vuc3ltMwYAAAAAAAAAB3Bpbmlwb3AAAAAAAAAAAAAHZ2Vuc3ltMwAAAAAAAAAACyRkZWNsdGVtcCQ1AAAAAAAAAAABAAAAAAAAAAALJGRlY2x0ZW1wJDc=";
this.export = function ($env,$$dummy) {
  const $$$env1 = new rt.Env();
  const inputLineWithPini0 = rt.mkVal(new rt.Closure($$$env1, this, this.inputLineWithPini0))
  $$$env1.inputLineWithPini0 = inputLineWithPini0;
  $$$env1.inputLineWithPini0.selfpointer = true;
  const $$$env2 = new rt.Env();
  $$$env2.inputLineWithPini0 = inputLineWithPini0;
  const inputLineAtLevel11 = rt.mkVal(new rt.Closure($$$env2, this, this.inputLineAtLevel11))
  $$$env2.inputLineAtLevel11 = inputLineAtLevel11;
  $$$env2.inputLineAtLevel11.selfpointer = true;
  const gensym32 = rt.mkValPos ("inputLineWithPini",'');;
  const gensym33 = rt.mkVal(rt.mkTuple([gensym32, inputLineWithPini0]));
  const gensym34 = rt.mkValPos ("inputLineAtLevel",'');;
  const gensym35 = rt.mkVal(rt.mkTuple([gensym34, inputLineAtLevel11]));
  const gensym36 = rt.mkVal(rt.mkList([gensym33, gensym35]));
  return (gensym36);
}
this.export.deps = ['inputLineWithPini0', 'inputLineAtLevel11'];
this.export.libdeps = [];
this.export.serialized = "AAAAAAAAAAAGZXhwb3J0AAAAAAAAAAckJGR1bW15AAAAAAAAAAcBAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAASaW5wdXRMaW5lV2l0aFBpbmkwAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTABAAAAAAAAAAEAAAAAAAAAEmlucHV0TGluZVdpdGhQaW5pMAAAAAAAAAAAEmlucHV0TGluZVdpdGhQaW5pMAAAAAAAAAABAAAAAAAAABJpbnB1dExpbmVBdExldmVsMTEAAAAAAAAAEmlucHV0TGluZUF0TGV2ZWwxMQAAAAAAAAAACGdlbnN5bTMyBQEAAAAAAAAAEWlucHV0TGluZVdpdGhQaW5pAAAAAAAAAAAIZ2Vuc3ltMzMCAAAAAAAAAAIAAAAAAAAAAAhnZW5zeW0zMgAAAAAAAAAAEmlucHV0TGluZVdpdGhQaW5pMAAAAAAAAAAACGdlbnN5bTM0BQEAAAAAAAAAEGlucHV0TGluZUF0TGV2ZWwAAAAAAAAAAAhnZW5zeW0zNQIAAAAAAAAAAgAAAAAAAAAACGdlbnN5bTM0AAAAAAAAAAASaW5wdXRMaW5lQXRMZXZlbDExAAAAAAAAAAAIZ2Vuc3ltMzYDAAAAAAAAAAIAAAAAAAAAAAhnZW5zeW0zMwAAAAAAAAAACGdlbnN5bTM1BAAAAAAAAAAACGdlbnN5bTM2";
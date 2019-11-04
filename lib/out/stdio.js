this.uuid = rt.rt_uuid
this.libSet = new Set ()
this.libs = []
this.addLib = function (lib, decl) { if (!this.libSet.has (lib +'.'+decl)) { this.libSet.add (lib +'.'+decl); this.libs.push ({lib:lib, decl:decl})} }
this.loadlibs = function (cb) { rt.linkLibs (this.libs, this, cb) }
this.serializedatoms = "AQAAAAAAAAAA"
this.gensym15 = function ($env,inputLineAtLevel_arg213) {
  rt.push (($decltemp$18) =>
           {const gensym16 = rt.mkCopy(rt.declassify);
            const gensym17 = rt.mkVal(rt.mkTuple([$decltemp$18, $env.inputLineAtLevel_arg112, inputLineAtLevel_arg213]));
            rt.tailcall (gensym16,gensym17);});
  rt.tailcall ($env.inputLineWithPini0,$env.inputLineAtLevel_arg112);
}
this.gensym15.deps = [];
this.gensym15.serialized = "AAAAAAAAAAAIZ2Vuc3ltMTUAAAAAAAAAF2lucHV0TGluZUF0TGV2ZWxfYXJnMjEzAAAAAAAAAAAGAAAAAAAAAAwkZGVjbHRlbXAkMTgAAAAAAAAAAAABAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTABAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAACAAAAAAAAAAAIZ2Vuc3ltMTYGAAAAAAAAAApkZWNsYXNzaWZ5AAAAAAAAAAAIZ2Vuc3ltMTcCAAAAAAAAAAMAAAAAAAAAAAwkZGVjbHRlbXAkMTgBAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAAAF2lucHV0TGluZUF0TGV2ZWxfYXJnMjEzAAAAAAAAAAAACGdlbnN5bTE2AAAAAAAAAAAIZ2Vuc3ltMTc=";
this.inputLineAtLevel11 = function ($env,inputLineAtLevel_arg112) {
  const $$$env0 = new rt.Env();
  $$$env0.inputLineAtLevel_arg112 = inputLineAtLevel_arg112;
  $$$env0.inputLineWithPini0 = $env.inputLineWithPini0;
  const gensym15 = rt.mkVal(new rt.Closure($$$env0, this, this.gensym15))
  $$$env0.gensym15 = gensym15;
  $$$env0.gensym15.selfpointer = true;
  rt.ret (gensym15);
}
this.inputLineAtLevel11.deps = ['gensym15'];
this.inputLineAtLevel11.serialized = "AAAAAAAAAAASaW5wdXRMaW5lQXRMZXZlbDExAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAABAQAAAAAAAAACAAAAAAAAABdpbnB1dExpbmVBdExldmVsX2FyZzExMgAAAAAAAAAAF2lucHV0TGluZUF0TGV2ZWxfYXJnMTEyAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTABAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTAAAAAAAAAAAQAAAAAAAAAIZ2Vuc3ltMTUAAAAAAAAACGdlbnN5bTE1AQAAAAAAAAAACGdlbnN5bTE1";
this.inputLineWithPini0 = function ($env,inputLineWithPini_arg11) {
  rt.push (($decltemp$5) =>
           {rt.push (($decltemp$7) =>
                     {rt.push (($decltemp$9) =>
                               {rt.ret ($decltemp$7);});
                      const gensym1 = rt.mkCopy(rt.pinipop);
                      rt.tailcall (gensym1,$decltemp$5);});
            const gensym2 = rt.mkCopy(rt.inputLine);
            const gensym3 = rt.__unit;
            rt.tailcall (gensym2,gensym3);});
  const gensym4 = rt.mkCopy(rt.pinipush);
  rt.tailcall (gensym4,inputLineWithPini_arg11);
}
this.inputLineWithPini0.deps = [];
this.inputLineWithPini0.serialized = "AAAAAAAAAAASaW5wdXRMaW5lV2l0aFBpbmkwAAAAAAAAABdpbnB1dExpbmVXaXRoUGluaV9hcmcxMQAAAAAAAAAABgAAAAAAAAALJGRlY2x0ZW1wJDUAAAAAAAAAAQAAAAAAAAAAB2dlbnN5bTQGAAAAAAAAAAhwaW5pcHVzaAAAAAAAAAAAAAdnZW5zeW00AAAAAAAAAAAXaW5wdXRMaW5lV2l0aFBpbmlfYXJnMTEAAAAAAAAAAAYAAAAAAAAACyRkZWNsdGVtcCQ3AAAAAAAAAAIAAAAAAAAAAAdnZW5zeW0yBgAAAAAAAAAJaW5wdXRMaW5lAAAAAAAAAAAHZ2Vuc3ltMwUDAAAAAAAAAAAAB2dlbnN5bTIAAAAAAAAAAAdnZW5zeW0zAAAAAAAAAAAGAAAAAAAAAAskZGVjbHRlbXAkOQAAAAAAAAABAAAAAAAAAAAHZ2Vuc3ltMQYAAAAAAAAAB3Bpbmlwb3AAAAAAAAAAAAAHZ2Vuc3ltMQAAAAAAAAAACyRkZWNsdGVtcCQ1AAAAAAAAAAABAAAAAAAAAAALJGRlY2x0ZW1wJDc=";
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
  const gensym30 = rt.mkValPos ("inputLineWithPini",'');;
  const gensym31 = rt.mkVal(rt.mkTuple([gensym30, inputLineWithPini0]));
  const gensym32 = rt.mkValPos ("inputLineAtLevel",'');;
  const gensym33 = rt.mkVal(rt.mkTuple([gensym32, inputLineAtLevel11]));
  const gensym34 = rt.mkVal(rt.mkList([gensym31, gensym33]));
  return (gensym34);
}
this.export.deps = ['inputLineWithPini0', 'inputLineAtLevel11'];
this.export.serialized = "AAAAAAAAAAAGZXhwb3J0AAAAAAAAAAckJGR1bW15AAAAAAAAAAcBAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAASaW5wdXRMaW5lV2l0aFBpbmkwAAAAAAAAABJpbnB1dExpbmVXaXRoUGluaTABAAAAAAAAAAEAAAAAAAAAEmlucHV0TGluZVdpdGhQaW5pMAAAAAAAAAAAEmlucHV0TGluZVdpdGhQaW5pMAAAAAAAAAABAAAAAAAAABJpbnB1dExpbmVBdExldmVsMTEAAAAAAAAAEmlucHV0TGluZUF0TGV2ZWwxMQAAAAAAAAAACGdlbnN5bTMwBQEAAAAAAAAAEWlucHV0TGluZVdpdGhQaW5pAAAAAAAAAAAIZ2Vuc3ltMzECAAAAAAAAAAIAAAAAAAAAAAhnZW5zeW0zMAAAAAAAAAAAEmlucHV0TGluZVdpdGhQaW5pMAAAAAAAAAAACGdlbnN5bTMyBQEAAAAAAAAAEGlucHV0TGluZUF0TGV2ZWwAAAAAAAAAAAhnZW5zeW0zMwIAAAAAAAAAAgAAAAAAAAAACGdlbnN5bTMyAAAAAAAAAAASaW5wdXRMaW5lQXRMZXZlbDExAAAAAAAAAAAIZ2Vuc3ltMzQDAAAAAAAAAAIAAAAAAAAAAAhnZW5zeW0zMQAAAAAAAAAACGdlbnN5bTMzBAAAAAAAAAAACGdlbnN5bTM0";
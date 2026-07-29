function Top (rt) {
  this.__consumedDatatypeHashes = {"Outcome":["k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g"]};
  this.__moduleRoot = "trp-compiler"
  this.__moduleDepsFile = "trp-compiler/roundtrip.deps.json"
  this.libSet = new Set ()
  this.libs = []
  this.addLib = function (lib, decl) { if (!this.libSet.has (lib +'.'+decl)) { this.libSet.add (lib +'.'+decl); this.libs.push ({lib:lib, decl:decl})} }
  this.addLib  ("SimpleFileIO" , "readFile")
  this.addLib  ("module:6mcbrainqkmree4nrdu16nkmimg495d6mspbpbtk5dum1is8cfhg" , "fromString")
  this.addLib  ("SimpleFileIO" , "writeFile")
  this.addLib  ("module:6mcbrainqkmree4nrdu16nkmimg495d6mspbpbtk5dum1is8cfhg" , "toText")
  this.fwriteln2 = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  6
    _T.updateSparseBitOnEntry($env.__dataLevel)
    const gensym16$$$const = 2
    const gensym4$$$const = "\n"
    const gensym13$$$const = "pattern match failure in function fwriteln"
    const gensym18$$$const = false
    const _$reg0_val_0 = _T.r0_val;
    _STACK[ _SP + 1] =  _$reg0_val_0
    let _$reg0_vlbl_1 = _T.pc;
    let _$reg0_tlbl_2 = _T.pc;
    let _pc_3 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      _$reg0_vlbl_1 = _T.r0_lev;
      _$reg0_tlbl_2 = _T.r0_tlev;
      _pc_3 = _T.pc;
    }
    _STACK[ _SP + 2] =  _$reg0_vlbl_1
    _STACK[ _SP + 0] =  _$reg0_tlbl_2
    _STACK[ _SP + 3] =  _pc_3
    const gensym4 = rt.constructLVal (gensym4$$$const,_pc_3,_pc_3);
    _STACK[ _SP + 5] =  gensym4
    const _raw_5 = rt.raw_istuple(_$reg0_val_0);
    let _lbl_8 = _T.pc;
    let _bl_18 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      _lbl_8 = rt.raw_join (_pc_3,_$reg0_vlbl_1);;
      const _bl_17 = _T.bl;
      _bl_18 = rt.raw_join (_bl_17,_lbl_8);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  12 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$fwriteln2$$$kont1
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_raw_5) {
      rt.rawAssertIsTuple (_$reg0_val_0);
      const _raw_24 = rt.raw_tupleLength(_$reg0_val_0);
      const lval35$val_opt = _raw_24 === gensym16$$$const;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_21 = rt.raw_join (_bl_18,_$reg0_tlbl_2);;
        _T.bl = _bl_21;
      }
      _T.r0_val = lval35$val_opt;
      _T.r0_lev = _lbl_8;
      _T.r0_tlev = _lbl_8;
      return _T.returnImmediate ();
    } else {
      if (! _STACK[ _SP +  -6] ) {
        _T.bl = _bl_18;
      }
      _T.r0_val = gensym18$$$const;
      _T.r0_lev = _lbl_8;
      _T.r0_tlev = _lbl_8;
      return _T.returnImmediate ();
    }
  }
  this.fwriteln2.deps = [];
  this.fwriteln2.libdeps = [];
  this.fwriteln2.serialized = "VFJQSQIfiwgAAAAAAAATlVPLTsMwELz3K1ZWD84hUl8p4YaEekDiyLESclI3NThOsDei/D2bpHmXqtxs73hmPLvmaLMil76yvpPnfAarGfBjYWYA7PhtFUptVox2XNikO3qn3TJkXlmIM+PQ0Qo4S6RxP+lyy4Arg7DyvMF5SOdRlmk4Cu3kqLimokOrDOnkAlFaA6nA+ERopQsrQRkgazGqjBYXJ2zIsumR7PdltfIYRRWohjoU8adPrxXmUB5A46B6aIumBRfOqcS0gEdiLwy8uLci1xK4zmKhp6nUoiWBOtaLFtoSeU2hEbsiF9RyldirNAmebkg2LHBhafct3YZ1ZzxSPQTsvnqbkdeAef3iE3CLwJ6FkzutUmVE2Q/mja9tqxzaq9xKHEM2Pd+UA/XnKizsMXW9aSKm10qLvqSB8qW1mb2e+OquxBeUeG6zD18dzn9mDYtp2g1DeBfBsh/MZByrns0PMtYo03xOH6NT67xP3Ze/LqK2NILMGzRuCH4gMA6muI1g3Mr+RFdMSP/RJ4AeAbeTqw/Dybzlfv0f98H97jf3ul9PrgaDER4presilX8BXvpHiEQFAAA=";
  this.fwriteln2.framesize = 6;
  this.fwritelnWithLabels3 = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  6
    _T.updateSparseBitOnEntry($env.__dataLevel)
    const gensym41$$$const = 2
    const gensym38$$$const = "pattern match failure in function fwritelnWithLabels"
    const gensym43$$$const = false
    _STACK[ _SP + 5] =  $env
    const _$reg0_val_0 = _T.r0_val;
    _STACK[ _SP + 1] =  _$reg0_val_0
    const _raw_5 = rt.raw_istuple(_$reg0_val_0);
    let _$reg0_vlbl_1 = _T.pc;
    let _$reg0_tlbl_2 = _T.pc;
    let _pc_3 = _T.pc;
    let _lbl_8 = _T.pc;
    let _bl_18 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      _$reg0_vlbl_1 = _T.r0_lev;
      _$reg0_tlbl_2 = _T.r0_tlev;
      _pc_3 = _T.pc;
      _lbl_8 = rt.raw_join (_pc_3,_$reg0_vlbl_1);;
      const _bl_17 = _T.bl;
      _bl_18 = rt.raw_join (_bl_17,_lbl_8);;
    }
    _STACK[ _SP + 2] =  _$reg0_vlbl_1
    _STACK[ _SP + 0] =  _$reg0_tlbl_2
    _STACK[ _SP + 3] =  _pc_3
    _SP_OLD = _SP; 
    _SP = _SP +  12 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$fwritelnWithLabels3$$$kont3
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_raw_5) {
      rt.rawAssertIsTuple (_$reg0_val_0);
      const _raw_24 = rt.raw_tupleLength(_$reg0_val_0);
      const lval35$val_opt = _raw_24 === gensym41$$$const;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_21 = rt.raw_join (_bl_18,_$reg0_tlbl_2);;
        _T.bl = _bl_21;
      }
      _T.r0_val = lval35$val_opt;
      _T.r0_lev = _lbl_8;
      _T.r0_tlev = _lbl_8;
      return _T.returnImmediate ();
    } else {
      if (! _STACK[ _SP +  -6] ) {
        _T.bl = _bl_18;
      }
      _T.r0_val = gensym43$$$const;
      _T.r0_lev = _lbl_8;
      _T.r0_tlev = _lbl_8;
      return _T.returnImmediate ();
    }
  }
  this.fwritelnWithLabels3.deps = [];
  this.fwritelnWithLabels3.libdeps = [];
  this.fwritelnWithLabels3.serialized = "VFJQSQIfiwgAAAAAAAATnVQ9T8MwEN37K06enCFSk7QCNiTEgNQNJEbkBLc1uHawL3z8ey5JkzhJQYXNvs/n9+7M0dmqlLFysZef5QLSBfBtZRYAbPvhFEptHhXuNyKX2meM7Fy43SnnE9mTZM2iOqawxqOnE3C2k8Z/HVYJA64MQhpFI3tG9txaDVuhvRw7s8u6I909OmWobSkQpTNwEFjsKUPpyklQBghzgcrSYQaMRU1RnudN6baBR1G8xvRkYZ6bFl3Hi2PHNpoOXHivdqa99XGrFessvDJw5x+qUkvg2hZC/0pPi6bOU9tjiS6rLx11jg7FHMeAZMkGW42lQbKRZof78/D06T+1yK7CFrkKIuD2LbhMHrJkUei8Bu4Q2I3w8largzKiVoxF07SkIalP5U7iJIQADQFEEql6Koxma6g0KNrxT6+VDmMiQ8bSOetOykET8Rc5snXIVensS6yeP88RApbnSJH9u3wyL99XTeslJF2Aob1vdm0TTsZ8XUJIwQB2WiBtZkyI9JTMdCZ3lo1ncOB4YLmPrT8RHG1az/qs7pKFUwQjUNK8D2SdwDQewYnzsnWS+xtARJ+wPwUAAA==";
  this.fwritelnWithLabels3.framesize = 6;
  this.printString4 = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  2
    _T.updateSparseBitOnEntry($env.__dataLevel)
    _STACK[ _SP + 1] =  $env
    const _$reg0_val_0 = _T.r0_val;
    let _$reg0_vlbl_1 = _T.pc;
    let _$reg0_tlbl_2 = _T.pc;
    let _pc_3 = _T.pc;
    if (! _STACK[ _SP +  2] ) {
      _$reg0_vlbl_1 = _T.r0_lev;
      _$reg0_tlbl_2 = _T.r0_tlev;
      _pc_3 = _T.pc;
    }
    const printString_arg120 = rt.constructLVal (_$reg0_val_0,_$reg0_vlbl_1,_$reg0_tlbl_2);
    _STACK[ _SP + 0] =  printString_arg120
    const _raw_4 = rt. stdout;
    rt.rawAssertIsFunction (_raw_4);
    const _val_20 = $env.gensym204.val;
    const _vlbl_21 = $env.gensym204.lev;
    const _tlbl_22 = $env.gensym204.tlev;
    let _bl_15 = _T.pc;
    if (! _STACK[ _SP +  2] ) {
      const _bl_14 = _T.bl;
      _bl_15 = rt.raw_join (_bl_14,_pc_3);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  8 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$printString4$$$kont4
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    if (! _STACK[ _SP +  -6] ) {
      _T.pc = _pc_3;
      _T.bl = _bl_15;
    }
    _T.r0_val = _val_20;
    _T.r0_lev = _vlbl_21;
    _T.r0_tlev = _tlbl_22;
    return _raw_4
  }
  this.printString4.deps = [];
  this.printString4.libdeps = [];
  this.printString4.serialized = "VFJQSQIfiwgAAAAAAAATbZDdDsIgDIXv9xRN4wVcLJlEfREfwOBWFyIyQjt/3l62KdEoF6V87TltUJKGMVLtUs10jxWYCtRpDBUAxuSC7CXHfoMZKJv6L3rIYG0a1FOxHQILz+nxmCMoPUcW257r7G1DNwHAVUetF7rElZl9iyInyjK7PgD2FPhx2e4wFy0TIEs3jIJa61erWOfr1noPyg/5/tBoUBSub2CaTZH9TFpeUMRbfBMlY/RUzL+21gX/+46/O84LnW7JCflgPhzKYL2c6glTGZ/7lAEAAA==";
  this.printString4.framesize = 2;
  this.print5 = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  1
    _T.updateSparseBitOnEntry($env.__dataLevel)
    _STACK[ _SP + 0] =  $env
    const _$reg0_val_0 = _T.r0_val;
    const _raw_4 = rt. toString;
    rt.rawAssertIsFunction (_raw_4);
    let _$reg0_vlbl_1 = _T.pc;
    let _$reg0_tlbl_2 = _T.pc;
    let _pc_3 = _T.pc;
    let _bl_15 = _T.pc;
    if (! _STACK[ _SP +  1] ) {
      _$reg0_vlbl_1 = _T.r0_lev;
      _$reg0_tlbl_2 = _T.r0_tlev;
      _pc_3 = _T.pc;
      const _bl_14 = _T.bl;
      _bl_15 = rt.raw_join (_bl_14,_pc_3);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  7 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$print5$$$kont5
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    if (! _STACK[ _SP +  -6] ) {
      _T.pc = _pc_3;
      _T.bl = _bl_15;
    }
    _T.r0_val = _$reg0_val_0;
    _T.r0_lev = _$reg0_vlbl_1;
    _T.r0_tlev = _$reg0_tlbl_2;
    return _raw_4
  }
  this.print5.deps = [];
  this.print5.libdeps = [];
  this.print5.serialized = "VFJQSQIfiwgAAAAAAAATbY5LDsMgDET3nMLyyixYNOrvHj1ARVKKUClEmFTt7et8FKVSWcB4sN+YaslD70woht27V9AooPuQFAD2JaR6QJFki1/qq8hdc0Q92l1OXHmSbSs3EFnm4BOgd4k/z+MZ5cuyA6z5UgXgUWs9tXK13cNIqE230VhnTjiVggTSQNWGaDobI1DM8m7YerV+d5sT/hFcei3N8zb7DWON1/NRX0XikBQdAQAA";
  this.print5.framesize = 1;
  this.printWithLabels6 = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  1
    _T.updateSparseBitOnEntry($env.__dataLevel)
    _STACK[ _SP + 0] =  $env
    const _$reg0_val_0 = _T.r0_val;
    const _raw_4 = rt. toStringL;
    rt.rawAssertIsFunction (_raw_4);
    let _$reg0_vlbl_1 = _T.pc;
    let _$reg0_tlbl_2 = _T.pc;
    let _pc_3 = _T.pc;
    let _bl_15 = _T.pc;
    if (! _STACK[ _SP +  1] ) {
      _$reg0_vlbl_1 = _T.r0_lev;
      _$reg0_tlbl_2 = _T.r0_tlev;
      _pc_3 = _T.pc;
      const _bl_14 = _T.bl;
      _bl_15 = rt.raw_join (_bl_14,_pc_3);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  7 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$printWithLabels6$$$kont6
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    if (! _STACK[ _SP +  -6] ) {
      _T.pc = _pc_3;
      _T.bl = _bl_15;
    }
    _T.r0_val = _$reg0_val_0;
    _T.r0_lev = _$reg0_vlbl_1;
    _T.r0_tlev = _$reg0_tlbl_2;
    return _raw_4
  }
  this.printWithLabels6.deps = [];
  this.printWithLabels6.libdeps = [];
  this.printWithLabels6.serialized = "VFJQSQIfiwgAAAAAAAATdY67DsIwDEX3fEXkKRkilYd4/EM3BkaUlhAiQlLFKYK/x31QFSEyxPa1fXxFTrFtjHJJoXk2jC8ZF5c2MM6hSS7ko8vXUlfG4wZIFDrZn86JxMWqANkN1DFgxj6tKvq5EBrR2cDBmoCv+64Aamk0HHI8ZELZEqSU/SxmXd8UOdHh3Amfpe0e+nJkUiLHmLXzqtbec+EjxdkZOUn/DA9XOyoBv1gmPMa1weJ6RpssyeGxN0ZPUthGAQAA";
  this.printWithLabels6.framesize = 1;
  this.inputLine7 = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  3
    _T.updateSparseBitOnEntry($env.__dataLevel)
    _STACK[ _SP + 2] =  $env
    const _raw_4 = rt. stdin;
    rt.rawAssertIsFunction (_raw_4);
    const _val_20 = $env.gensym204.val;
    const _vlbl_21 = $env.gensym204.lev;
    const _tlbl_22 = $env.gensym204.tlev;
    let _pc_3 = _T.pc;
    let _bl_15 = _T.pc;
    if (! _STACK[ _SP +  3] ) {
      _pc_3 = _T.pc;
      const _bl_14 = _T.bl;
      _bl_15 = rt.raw_join (_bl_14,_pc_3);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  9 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$inputLine7$$$kont8
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    if (! _STACK[ _SP +  -6] ) {
      _T.pc = _pc_3;
      _T.bl = _bl_15;
    }
    _T.r0_val = _val_20;
    _T.r0_lev = _vlbl_21;
    _T.r0_tlev = _tlbl_22;
    return _raw_4
  }
  this.inputLine7.deps = [];
  this.inputLine7.libdeps = [];
  this.inputLine7.serialized = "VFJQSQIfiwgAAAAAAAATjZDbCsIwDIbv9xQheNFeDNwB1HfwHaTb4ijWbCyd6NvbTh0eUOxFmvzJl6RVfujGnlI7pELnPoE8AbUfOQFAy/3ot5ZphSFUZmiftF0Is6JEHVN1x+JlcqsqWFB6suJNfUhDX8NNFAAXDdXO07FfFGucpDsRHGVEbMuALbFcjpsSQ9IIAYpvLKPW+l7pjXVpbZwD5bpwPyEaFPHpIeTLcsa+D8rmQfuBTOPiqFspvJcWX3f6eCvMUI6PblUVvubH/sWP/f/As4i/Snnkbye5AijLhx7tAQAA";
  this.inputLine7.framesize = 3;
  this.main = ($env) => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot =  _SP +  30
    _T.updateSparseBitOnEntry($env.__dataLevel)
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_val_0 = _T.r0_val;
    let _pc_3 = _T.pc;
    let _lbl_8 = _T.pc;
    let _lbl_10 = _T.pc;
    if (! _STACK[ _SP +  30] ) {
      const _$reg0_vlbl_1 = _T.r0_lev;
      const _$reg0_tlbl_2 = _T.r0_tlev;
      _pc_3 = _T.pc;
      _lbl_8 = rt.raw_join (_pc_3,_$reg0_vlbl_1);;
      _lbl_10 = rt.raw_join (_pc_3,_$reg0_tlbl_2);;
    }
    _STACK[ _SP + 19] =  _pc_3
    const gensym204 = rt.constructLVal (_$reg0_val_0,_lbl_8,_lbl_10);
    _STACK[ _SP + 28] =  gensym204
    const $$$env9 = new rt.Env();
    $$$env9.gensym204 = gensym204;
    $$$env9.__dataLevel =  rt.raw_join (gensym204.dataLevel);
    const fwriteln2 = rt.mkVal(rt.RawClosure($$$env9, this, this.fwriteln2))
    $$$env9.fwriteln2 = fwriteln2;
    $$$env9.fwriteln2.selfpointer = true;
    const fwritelnWithLabels3 = rt.mkVal(rt.RawClosure($$$env9, this, this.fwritelnWithLabels3))
    $$$env9.fwritelnWithLabels3 = fwritelnWithLabels3;
    $$$env9.fwritelnWithLabels3.selfpointer = true;
    const printString4 = rt.mkVal(rt.RawClosure($$$env9, this, this.printString4))
    $$$env9.printString4 = printString4;
    $$$env9.printString4.selfpointer = true;
    const print5 = rt.mkVal(rt.RawClosure($$$env9, this, this.print5))
    $$$env9.print5 = print5;
    $$$env9.print5.selfpointer = true;
    const printWithLabels6 = rt.mkVal(rt.RawClosure($$$env9, this, this.printWithLabels6))
    $$$env9.printWithLabels6 = printWithLabels6;
    $$$env9.printWithLabels6.selfpointer = true;
    const inputLine7 = rt.mkVal(rt.RawClosure($$$env9, this, this.inputLine7))
    $$$env9.inputLine7 = inputLine7;
    $$$env9.inputLine7.selfpointer = true;
    _STACK[ _SP + 29] =  print5
    const _raw_11 = rt. getCliArgs;
    rt.rawAssertIsFunction (_raw_11);
    let _bl_22 = _T.pc;
    if (! _STACK[ _SP +  30] ) {
      const _bl_21 = _T.bl;
      _bl_22 = rt.raw_join (_bl_21,_pc_3);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  36 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont28
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.r0_val = _$reg0_val_0;
    _T.r0_lev = _lbl_8;
    _T.r0_tlev = _lbl_10;
    if (! _STACK[ _SP +  -6] ) {
      _T.pc = _pc_3;
      _T.bl = _bl_22;
    }
    return _raw_11
  }
  this.main.deps = ["fwriteln2", "fwritelnWithLabels3", "printString4", "print5", "printWithLabels6", "inputLine7"];
  this.main.libdeps = ["SimpleFileIO", "module:6mcbrainqkmree4nrdu16nkmimg495d6mspbpbtk5dum1is8cfhg"];
  this.main.serialized = "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";
  this.main.framesize = 30;
  this.$$$fwriteln2$$$kont0 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  6
    _T.updateSparseBitOnReturn()
    const gensym16$$$const = 2
    const gensym4$$$const = "\n"
    const gensym13$$$const = "pattern match failure in function fwriteln"
    const gensym18$$$const = false
    const gensym10 = _STACK[ _SP + 4]
    const gensym4 = _STACK[ _SP + 5]
    const _raw_161 = rt. fwrite;
    const _raw_168 = rt.mkTuple([gensym10, gensym4], false);
    rt.rawAssertIsFunction (_raw_161);
    let _pc_162 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      _pc_162 = _T.pc;
      const _bl_178 = _T.bl;
      const _bl_179 = rt.raw_join (_bl_178,_pc_162);;
      _T.pc = _pc_162;
      _T.bl = _bl_179;
    }
    _T.r0_val = _raw_168;
    _T.r0_lev = _pc_162;
    _T.r0_tlev = _pc_162;
    return _raw_161
  }
  this.$$$fwriteln2$$$kont0.debugname = "$$$fwriteln2$$$kont0"
  this.$$$fwriteln2$$$kont1 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  6
    _T.updateSparseBitOnReturn()
    const gensym16$$$const = 2
    const gensym4$$$const = "\n"
    const gensym13$$$const = "pattern match failure in function fwriteln"
    const gensym18$$$const = false
    const _$reg0_tlbl_2 = _STACK[ _SP + 0]
    const _$reg0_val_0 = _STACK[ _SP + 1]
    const _$reg0_vlbl_1 = _STACK[ _SP + 2]
    const _pc_3 = _STACK[ _SP + 3]
    const _$reg0_val_202 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_202);
    let _bl_83 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      const _$reg0_vlbl_203 = _T.r0_lev;
      const _$reg0_tlbl_204 = _T.r0_tlev;
      const _bl_78 = _T.bl;
      const _bl_79 = rt.raw_join (_bl_78,_$reg0_tlbl_204);;
      _bl_83 = rt.raw_join (_bl_79,_$reg0_vlbl_203);;
    }
    if (_$reg0_val_202) {
      rt.rawAssertIsTuple (_$reg0_val_0);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_0,0);
      const lval93 = rt.raw_indexTuple(_$reg0_val_0,0);
      const _val_94 = lval93.val;
      const _vlbl_95 = lval93.lev;
      const _tlbl_96 = lval93.tlev;
      let _bl_90 = _T.pc;
      let _pc_97 = _T.pc;
      let _lbl_99 = _T.pc;
      let _lbl_100 = _T.pc;
      let _lbl_104 = _T.pc;
      if (! _STACK[ _SP +  6] ) {
        const _bl_86 = rt.raw_join (_bl_83,_$reg0_tlbl_2);;
        _bl_90 = rt.raw_join (_bl_86,_$reg0_vlbl_1);;
        _pc_97 = _T.pc;
        _lbl_99 = rt.raw_join (_pc_97,_$reg0_vlbl_1);;
        _lbl_100 = rt.raw_join (_lbl_99,_vlbl_95);;
        const _lbl_103 = rt.raw_join (_pc_97,_tlbl_96);;
        _lbl_104 = rt.raw_join (_lbl_103,_$reg0_vlbl_1);;
      }
      const gensym10 = rt.constructLVal (_val_94,_lbl_100,_lbl_104);
      _STACK[ _SP + 4] =  gensym10
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_0,1);
      const lval118 = rt.raw_indexTuple(_$reg0_val_0,1);
      const _val_119 = lval118.val;
      const _vlbl_120 = lval118.lev;
      const _tlbl_121 = lval118.tlev;
      let _lbl_125 = _T.pc;
      let _lbl_129 = _T.pc;
      if (! _STACK[ _SP +  6] ) {
        _lbl_125 = rt.raw_join (_lbl_99,_vlbl_120);;
        const _lbl_128 = rt.raw_join (_pc_97,_tlbl_121);;
        _lbl_129 = rt.raw_join (_lbl_128,_$reg0_vlbl_1);;
      }
      const gensym8 = rt.constructLVal (_val_119,_lbl_125,_lbl_129);
      const _raw_134 = rt. fwrite;
      const _raw_141 = rt.mkTuple([gensym10, gensym8], false);
      rt.rawAssertIsFunction (_raw_134);
      let _bl_152 = _T.pc;
      if (! _STACK[ _SP +  6] ) {
        _bl_152 = rt.raw_join (_bl_90,_pc_97);;
        _T.bl = _bl_90;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  12 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$fwriteln2$$$kont0
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_97;
        _T.bl = _bl_152;
      }
      _T.r0_val = _raw_141;
      _T.r0_lev = _pc_97;
      _T.r0_tlev = _pc_97;
      return _raw_134
    } else {
      if (! _STACK[ _SP +  6] ) {
        const _bl_193 = rt.raw_join (_bl_83,_pc_3);;
        const _pc_196 = _T.pc;
        const _pc_197 = rt.raw_join (_pc_196,_pc_3);;
        _T.pc = _pc_197;
        _T.bl = _bl_193;
      }
      rt.rawErrorPos (gensym13$$$const,"");
    }
  }
  this.$$$fwriteln2$$$kont1.debugname = "$$$fwriteln2$$$kont1"
  this.$$$fwritelnWithLabels3$$$kont2 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  6
    _T.updateSparseBitOnReturn()
    const gensym41$$$const = 2
    const gensym38$$$const = "pattern match failure in function fwritelnWithLabels"
    const gensym43$$$const = false
    const gensym35 = _STACK[ _SP + 4]
    const $env = _STACK[ _SP + 5]
    const _$reg0_val_174 = _T.r0_val;
    let _$reg0_vlbl_175 = _T.pc;
    let _$reg0_tlbl_176 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      _$reg0_vlbl_175 = _T.r0_lev;
      _$reg0_tlbl_176 = _T.r0_tlev;
    }
    const gensym30 = rt.constructLVal (_$reg0_val_174,_$reg0_vlbl_175,_$reg0_tlbl_176);
    const _raw_154 = rt.mkTuple([gensym35, gensym30], false);
    const _vlbl_161 = $env.fwriteln2.lev;
    const _tlbl_166 = $env.fwriteln2.tlev;
    const _val_169 = $env.fwriteln2.val;
    rt.rawAssertIsFunction (_val_169);
    let _pc_155 = _T.pc;
    let _pc_163 = _T.pc;
    let _bl_168 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      _pc_155 = _T.pc;
      _pc_163 = rt.raw_join (_pc_155,_vlbl_161);;
      const _bl_164 = _T.bl;
      const _bl_165 = rt.raw_join (_bl_164,_vlbl_161);;
      _bl_168 = rt.raw_join (_bl_165,_tlbl_166);;
    }
    _T.r0_val = _raw_154;
    _T.r0_lev = _pc_155;
    _T.r0_tlev = _pc_155;
    const _val_173 = $env.fwriteln2.val;
    if (! _STACK[ _SP +  6] ) {
      _T.pc = _pc_163;
      _T.bl = _bl_168;
    }
    return _val_173
  }
  this.$$$fwritelnWithLabels3$$$kont2.debugname = "$$$fwritelnWithLabels3$$$kont2"
  this.$$$fwritelnWithLabels3$$$kont3 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  6
    _T.updateSparseBitOnReturn()
    const gensym41$$$const = 2
    const gensym38$$$const = "pattern match failure in function fwritelnWithLabels"
    const gensym43$$$const = false
    const _$reg0_tlbl_2 = _STACK[ _SP + 0]
    const _$reg0_val_0 = _STACK[ _SP + 1]
    const _$reg0_vlbl_1 = _STACK[ _SP + 2]
    const _pc_3 = _STACK[ _SP + 3]
    const _$reg0_val_188 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_188);
    let _bl_83 = _T.pc;
    if (! _STACK[ _SP +  6] ) {
      const _$reg0_vlbl_189 = _T.r0_lev;
      const _$reg0_tlbl_190 = _T.r0_tlev;
      const _bl_78 = _T.bl;
      const _bl_79 = rt.raw_join (_bl_78,_$reg0_tlbl_190);;
      _bl_83 = rt.raw_join (_bl_79,_$reg0_vlbl_189);;
    }
    if (_$reg0_val_188) {
      rt.rawAssertIsTuple (_$reg0_val_0);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_0,0);
      const lval93 = rt.raw_indexTuple(_$reg0_val_0,0);
      const _val_94 = lval93.val;
      const _vlbl_95 = lval93.lev;
      const _tlbl_96 = lval93.tlev;
      let _bl_90 = _T.pc;
      let _pc_97 = _T.pc;
      let _lbl_99 = _T.pc;
      let _lbl_100 = _T.pc;
      let _lbl_104 = _T.pc;
      if (! _STACK[ _SP +  6] ) {
        const _bl_86 = rt.raw_join (_bl_83,_$reg0_tlbl_2);;
        _bl_90 = rt.raw_join (_bl_86,_$reg0_vlbl_1);;
        _pc_97 = _T.pc;
        _lbl_99 = rt.raw_join (_pc_97,_$reg0_vlbl_1);;
        _lbl_100 = rt.raw_join (_lbl_99,_vlbl_95);;
        const _lbl_103 = rt.raw_join (_pc_97,_tlbl_96);;
        _lbl_104 = rt.raw_join (_lbl_103,_$reg0_vlbl_1);;
      }
      const gensym35 = rt.constructLVal (_val_94,_lbl_100,_lbl_104);
      _STACK[ _SP + 4] =  gensym35
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_0,1);
      const lval118 = rt.raw_indexTuple(_$reg0_val_0,1);
      const _val_119 = lval118.val;
      const _vlbl_120 = lval118.lev;
      const _tlbl_121 = lval118.tlev;
      const _raw_134 = rt. toStringL;
      rt.rawAssertIsFunction (_raw_134);
      let _lbl_125 = _T.pc;
      let _lbl_129 = _T.pc;
      let _bl_145 = _T.pc;
      if (! _STACK[ _SP +  6] ) {
        _lbl_125 = rt.raw_join (_lbl_99,_vlbl_120);;
        const _lbl_128 = rt.raw_join (_pc_97,_tlbl_121);;
        _lbl_129 = rt.raw_join (_lbl_128,_$reg0_vlbl_1);;
        _bl_145 = rt.raw_join (_bl_90,_pc_97);;
        _T.bl = _bl_90;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  12 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$fwritelnWithLabels3$$$kont2
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_97;
        _T.bl = _bl_145;
      }
      _T.r0_val = _val_119;
      _T.r0_lev = _lbl_125;
      _T.r0_tlev = _lbl_129;
      return _raw_134
    } else {
      if (! _STACK[ _SP +  6] ) {
        const _bl_179 = rt.raw_join (_bl_83,_pc_3);;
        const _pc_182 = _T.pc;
        const _pc_183 = rt.raw_join (_pc_182,_pc_3);;
        _T.pc = _pc_183;
        _T.bl = _bl_179;
      }
      rt.rawErrorPos (gensym38$$$const,"");
    }
  }
  this.$$$fwritelnWithLabels3$$$kont3.debugname = "$$$fwritelnWithLabels3$$$kont3"
  this.$$$printString4$$$kont4 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  2
    _T.updateSparseBitOnReturn()
    const printString_arg120 = _STACK[ _SP + 0]
    const $env = _STACK[ _SP + 1]
    const _$reg0_val_44 = _T.r0_val;
    let _$reg0_vlbl_45 = _T.pc;
    let _$reg0_tlbl_46 = _T.pc;
    if (! _STACK[ _SP +  2] ) {
      _$reg0_vlbl_45 = _T.r0_lev;
      _$reg0_tlbl_46 = _T.r0_tlev;
    }
    const $decltemp$24 = rt.constructLVal (_$reg0_val_44,_$reg0_vlbl_45,_$reg0_tlbl_46);
    const _raw_24 = rt.mkTuple([$decltemp$24, printString_arg120], false);
    const _vlbl_31 = $env.fwriteln2.lev;
    const _tlbl_36 = $env.fwriteln2.tlev;
    const _val_39 = $env.fwriteln2.val;
    rt.rawAssertIsFunction (_val_39);
    let _pc_25 = _T.pc;
    let _pc_33 = _T.pc;
    let _bl_38 = _T.pc;
    if (! _STACK[ _SP +  2] ) {
      _pc_25 = _T.pc;
      _pc_33 = rt.raw_join (_pc_25,_vlbl_31);;
      const _bl_34 = _T.bl;
      const _bl_35 = rt.raw_join (_bl_34,_vlbl_31);;
      _bl_38 = rt.raw_join (_bl_35,_tlbl_36);;
    }
    _T.r0_val = _raw_24;
    _T.r0_lev = _pc_25;
    _T.r0_tlev = _pc_25;
    const _val_43 = $env.fwriteln2.val;
    if (! _STACK[ _SP +  2] ) {
      _T.pc = _pc_33;
      _T.bl = _bl_38;
    }
    return _val_43
  }
  this.$$$printString4$$$kont4.debugname = "$$$printString4$$$kont4"
  this.$$$print5$$$kont5 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  1
    _T.updateSparseBitOnReturn()
    const $env = _STACK[ _SP + 0]
    const _$reg0_val_37 = _T.r0_val;
    const _vlbl_24 = $env.printString4.lev;
    const _tlbl_29 = $env.printString4.tlev;
    const _val_32 = $env.printString4.val;
    rt.rawAssertIsFunction (_val_32);
    let _$reg0_vlbl_38 = _T.pc;
    let _$reg0_tlbl_39 = _T.pc;
    let _pc_26 = _T.pc;
    let _bl_31 = _T.pc;
    if (! _STACK[ _SP +  1] ) {
      _$reg0_vlbl_38 = _T.r0_lev;
      _$reg0_tlbl_39 = _T.r0_tlev;
      const _pc_25 = _T.pc;
      _pc_26 = rt.raw_join (_pc_25,_vlbl_24);;
      const _bl_27 = _T.bl;
      const _bl_28 = rt.raw_join (_bl_27,_vlbl_24);;
      _bl_31 = rt.raw_join (_bl_28,_tlbl_29);;
    }
    _T.r0_val = _$reg0_val_37;
    _T.r0_lev = _$reg0_vlbl_38;
    _T.r0_tlev = _$reg0_tlbl_39;
    const _val_36 = $env.printString4.val;
    if (! _STACK[ _SP +  1] ) {
      _T.pc = _pc_26;
      _T.bl = _bl_31;
    }
    return _val_36
  }
  this.$$$print5$$$kont5.debugname = "$$$print5$$$kont5"
  this.$$$printWithLabels6$$$kont6 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  1
    _T.updateSparseBitOnReturn()
    const $env = _STACK[ _SP + 0]
    const _$reg0_val_37 = _T.r0_val;
    const _vlbl_24 = $env.printString4.lev;
    const _tlbl_29 = $env.printString4.tlev;
    const _val_32 = $env.printString4.val;
    rt.rawAssertIsFunction (_val_32);
    let _$reg0_vlbl_38 = _T.pc;
    let _$reg0_tlbl_39 = _T.pc;
    let _pc_26 = _T.pc;
    let _bl_31 = _T.pc;
    if (! _STACK[ _SP +  1] ) {
      _$reg0_vlbl_38 = _T.r0_lev;
      _$reg0_tlbl_39 = _T.r0_tlev;
      const _pc_25 = _T.pc;
      _pc_26 = rt.raw_join (_pc_25,_vlbl_24);;
      const _bl_27 = _T.bl;
      const _bl_28 = rt.raw_join (_bl_27,_vlbl_24);;
      _bl_31 = rt.raw_join (_bl_28,_tlbl_29);;
    }
    _T.r0_val = _$reg0_val_37;
    _T.r0_lev = _$reg0_vlbl_38;
    _T.r0_tlev = _$reg0_tlbl_39;
    const _val_36 = $env.printString4.val;
    if (! _STACK[ _SP +  1] ) {
      _T.pc = _pc_26;
      _T.bl = _bl_31;
    }
    return _val_36
  }
  this.$$$printWithLabels6$$$kont6.debugname = "$$$printWithLabels6$$$kont6"
  this.$$$inputLine7$$$kont7 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  3
    _T.updateSparseBitOnReturn()
    const _pc_25 = _STACK[ _SP + 0]
    const _raw_24 = _STACK[ _SP + 1]
    const _$reg0_val_64 = _T.r0_val;
    rt.rawAssertIsFunction (_raw_24);
    let _$reg0_vlbl_65 = _T.pc;
    let _$reg0_tlbl_66 = _T.pc;
    if (! _STACK[ _SP +  3] ) {
      _$reg0_vlbl_65 = _T.r0_lev;
      _$reg0_tlbl_66 = _T.r0_tlev;
      const _pc_52 = _T.pc;
      const _pc_53 = rt.raw_join (_pc_52,_pc_25);;
      const _bl_54 = _T.bl;
      const _bl_55 = rt.raw_join (_bl_54,_pc_25);;
      _T.pc = _pc_53;
      _T.bl = _bl_55;
    }
    _T.r0_val = _$reg0_val_64;
    _T.r0_lev = _$reg0_vlbl_65;
    _T.r0_tlev = _$reg0_tlbl_66;
    return _raw_24
  }
  this.$$$inputLine7$$$kont7.debugname = "$$$inputLine7$$$kont7"
  this.$$$inputLine7$$$kont8 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  3
    _T.updateSparseBitOnReturn()
    const $env = _STACK[ _SP + 2]
    const _raw_24 = rt. freadln;
    _STACK[ _SP + 1] =  _raw_24
    const _raw_31 = rt. stdin;
    rt.rawAssertIsFunction (_raw_31);
    const _val_47 = $env.gensym204.val;
    const _vlbl_48 = $env.gensym204.lev;
    const _tlbl_49 = $env.gensym204.tlev;
    let _pc_25 = _T.pc;
    let _bl_42 = _T.pc;
    if (! _STACK[ _SP +  3] ) {
      _pc_25 = _T.pc;
      const _bl_41 = _T.bl;
      _bl_42 = rt.raw_join (_bl_41,_pc_25);;
    }
    _STACK[ _SP + 0] =  _pc_25
    _SP_OLD = _SP; 
    _SP = _SP +  9 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$inputLine7$$$kont7
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    if (! _STACK[ _SP +  -6] ) {
      _T.pc = _pc_25;
      _T.bl = _bl_42;
    }
    _T.r0_val = _val_47;
    _T.r0_lev = _vlbl_48;
    _T.r0_tlev = _tlbl_49;
    return _raw_31
  }
  this.$$$inputLine7$$$kont8.debugname = "$$$inputLine7$$$kont8"
  this.$$$main$$$kont10 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -11
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _pc_960 = _STACK[ _SP + -21]
    const _raw_972 = _STACK[ _SP + -17]
    const gensym193 = _STACK[ _SP + -14]
    const gensym204 = _STACK[ _SP + -13]
    const _$reg0_val_1019 = _T.r0_val;
    let _$reg0_vlbl_1020 = _T.pc;
    let _$reg0_tlbl_1021 = _T.pc;
    if (! _STACK[ _SP +  -11] ) {
      _$reg0_vlbl_1020 = _T.r0_lev;
      _$reg0_tlbl_1021 = _T.r0_tlev;
    }
    const gensym139 = rt.constructLVal (_$reg0_val_1019,_$reg0_vlbl_1020,_$reg0_tlbl_1021);
    const _raw_999 = rt.mkTuple([gensym204, gensym193, gensym139], false);
    rt.rawAssertIsFunction (_raw_972);
    let _pc_1000 = _T.pc;
    if (! _STACK[ _SP +  -11] ) {
      _pc_1000 = _T.pc;
      const _pc_1008 = rt.raw_join (_pc_1000,_pc_960);;
      const _bl_1009 = _T.bl;
      const _bl_1010 = rt.raw_join (_bl_1009,_pc_960);;
      _T.pc = _pc_1008;
      _T.bl = _bl_1010;
    }
    _T.r0_val = _raw_999;
    _T.r0_lev = _pc_1000;
    _T.r0_tlev = _pc_1000;
    return _raw_972
  }
  this.$$$main$$$kont10.debugname = "$$$main$$$kont10"
  this.$$$main$$$kont11 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1445 = _STACK[ _SP + -36]
    const _$reg0_val_1443 = _STACK[ _SP + -32]
    const _$reg0_vlbl_1444 = _STACK[ _SP + -28]
    const _pc_3 = _STACK[ _SP + -17]
    const print5 = _STACK[ _SP + -7]
    const _$reg0_val_1426 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1426);
    let _bl_1376 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1427 = _T.r0_lev;
      const _$reg0_tlbl_1428 = _T.r0_tlev;
      const _bl_1371 = _T.bl;
      const _bl_1372 = rt.raw_join (_bl_1371,_$reg0_tlbl_1428);;
      _bl_1376 = rt.raw_join (_bl_1372,_$reg0_vlbl_1427);;
    }
    if (_$reg0_val_1426) {
      rt.rawAssertIsTuple (_$reg0_val_1443);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1443,1);
      const _vlbl_1402 = print5.lev;
      const _tlbl_1407 = print5.tlev;
      const _val_1410 = print5.val;
      rt.rawAssertIsFunction (_val_1410);
      let _pc_1404 = _T.pc;
      let _bl_1409 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_1379 = rt.raw_join (_bl_1376,_$reg0_tlbl_1445);;
        const _bl_1383 = rt.raw_join (_bl_1379,_$reg0_vlbl_1444);;
        const _pc_1390 = _T.pc;
        _pc_1404 = rt.raw_join (_pc_1390,_vlbl_1402);;
        const _bl_1406 = rt.raw_join (_bl_1383,_vlbl_1402);;
        _bl_1409 = rt.raw_join (_bl_1406,_tlbl_1407);;
      }
      _T.r0_val = gensym113$$$const;
      _T.r0_lev = _pc_3;
      _T.r0_tlev = _pc_3;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_1404;
        _T.bl = _bl_1409;
      }
      return _val_1410
    } else {
      if (! _STACK[ _SP +  -6] ) {
        const _bl_1417 = rt.raw_join (_bl_1376,_pc_3);;
        const _pc_1420 = _T.pc;
        const _pc_1421 = rt.raw_join (_pc_1420,_pc_3);;
        _T.pc = _pc_1421;
        _T.bl = _bl_1417;
      }
      rt.rawErrorPos (gensym169$$$const,"trp-compiler/roundtrip.trp:17:25");
    }
  }
  this.$$$main$$$kont11.debugname = "$$$main$$$kont11"
  this.$$$main$$$kont12 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1445 = _STACK[ _SP + -36]
    const _$reg0_val_1443 = _STACK[ _SP + -32]
    const _$reg0_vlbl_1444 = _STACK[ _SP + -28]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1429 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1429);
    let _pc_1298 = _T.pc;
    let _bl_1367 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1430 = _T.r0_lev;
      const _$reg0_tlbl_1431 = _T.r0_tlev;
      const _pc_1297 = _T.pc;
      _pc_1298 = rt.raw_join (_pc_1297,_$reg0_vlbl_1430);;
      const _bl_1299 = _T.bl;
      const _bl_1300 = rt.raw_join (_bl_1299,_$reg0_vlbl_1430);;
      _bl_1367 = rt.raw_join (_bl_1300,_$reg0_tlbl_1431);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont11
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1429) {
      rt.rawAssertIsTuple (_$reg0_val_1443);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1443,0);
      const lval1310 = rt.raw_indexTuple(_$reg0_val_1443,0);
      const _val_1311 = lval1310.val;
      const _vlbl_1312 = lval1310.lev;
      const lval1328$val_opt = _val_1311 === gensym172$$$const;
      let _lbl_1336 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_1303 = rt.raw_join (_bl_1367,_$reg0_tlbl_1445);;
        const _bl_1307 = rt.raw_join (_bl_1303,_$reg0_vlbl_1444);;
        const _lbl_1316 = rt.raw_join (_pc_1298,_$reg0_vlbl_1444);;
        const _lbl_1317 = rt.raw_join (_lbl_1316,_vlbl_1312);;
        _lbl_1336 = rt.raw_join (_lbl_1317,_pc_3);;
        _T.bl = _bl_1307;
      }
      _T.r0_val = lval1328$val_opt;
      _T.r0_lev = _lbl_1336;
      _T.r0_tlev = _pc_1298;
      return _T.returnImmediate ();
    } else {
      let _lbl_1357 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_1357 = rt.raw_join (_pc_1298,_pc_3);;
        _T.bl = _bl_1367;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_1357;
      _T.r0_tlev = _lbl_1357;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont12.debugname = "$$$main$$$kont12"
  this.$$$main$$$kont13 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1445 = _STACK[ _SP + -36]
    const _$reg0_val_1443 = _STACK[ _SP + -32]
    const _$reg0_vlbl_1444 = _STACK[ _SP + -28]
    const _lbl_1026 = _STACK[ _SP + -24]
    const _lbl_156 = _STACK[ _SP + -21]
    const _lbl_160 = _STACK[ _SP + -20]
    const _pc_3 = _STACK[ _SP + -17]
    const _raw_1023 = _STACK[ _SP + -15]
    const _val_150 = _STACK[ _SP + -10]
    const print5 = _STACK[ _SP + -7]
    const _$reg0_val_1437 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1437);
    let _pc_1171 = _T.pc;
    let _bl_1434 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1438 = _T.r0_lev;
      const _$reg0_tlbl_1439 = _T.r0_tlev;
      const _pc_1170 = _T.pc;
      _pc_1171 = rt.raw_join (_pc_1170,_$reg0_vlbl_1438);;
      const _bl_1172 = _T.bl;
      const _bl_1173 = rt.raw_join (_bl_1172,_$reg0_vlbl_1438);;
      _bl_1434 = rt.raw_join (_bl_1173,_$reg0_tlbl_1439);;
    }
    _T.setBranchFlag()
    if (_$reg0_val_1437) {
      rt.rawAssertIsTuple (_$reg0_val_1443);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1443,1);
      rt.rawAssertIsString (_val_150);
      const _raw_1209 = gensym111$$$const + _val_150;
      const _vlbl_1220 = print5.lev;
      const _tlbl_1225 = print5.tlev;
      const _val_1228 = print5.val;
      rt.rawAssertIsFunction (_val_1228);
      let _lbl_1214 = _T.pc;
      let _pc_1222 = _T.pc;
      let _bl_1227 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_1176 = rt.raw_join (_bl_1434,_$reg0_tlbl_1445);;
        const _bl_1180 = rt.raw_join (_bl_1176,_$reg0_vlbl_1444);;
        const _bl_1201 = rt.raw_join (_bl_1180,_pc_3);;
        const _bl_1205 = rt.raw_join (_bl_1201,_lbl_160);;
        const _lbl_1213 = rt.raw_join (_pc_1171,_pc_3);;
        _lbl_1214 = rt.raw_join (_lbl_1213,_lbl_156);;
        _pc_1222 = rt.raw_join (_pc_1171,_vlbl_1220);;
        const _bl_1224 = rt.raw_join (_bl_1205,_vlbl_1220);;
        _bl_1227 = rt.raw_join (_bl_1224,_tlbl_1225);;
      }
      _T.r0_val = _raw_1209;
      _T.r0_lev = _lbl_1214;
      _T.r0_tlev = _pc_1171;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_1222;
        _T.bl = _bl_1227;
      }
      return _val_1228
    } else {
      let _pc_1235 = _T.pc;
      let _bl_1237 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _pc_1235 = rt.raw_join (_pc_1171,_lbl_1026);;
        _bl_1237 = rt.raw_join (_bl_1434,_lbl_1026);;
        _T.pc = _pc_1171;
        _T.bl = _bl_1434;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont12
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _T.setBranchFlag()
      if (_raw_1023) {
        rt.rawAssertIsTuple (_$reg0_val_1443);
        const _raw_1243 = rt.raw_tupleLength(_$reg0_val_1443);
        const lval1254$val_opt = _raw_1243 === gensym200$$$const;
        let _lbl_1262 = _T.pc;
        if (! _STACK[ _SP +  -11] ) {
          const _bl_1240 = rt.raw_join (_bl_1237,_$reg0_tlbl_1445);;
          _lbl_1262 = rt.raw_join (_pc_1235,_pc_3);;
          _T.bl = _bl_1240;
        }
        _T.r0_val = lval1254$val_opt;
        _T.r0_lev = _lbl_1262;
        _T.r0_tlev = _pc_1235;
        return _T.returnImmediate ();
      } else {
        let _lbl_1283 = _T.pc;
        if (! _STACK[ _SP +  -11] ) {
          _lbl_1283 = rt.raw_join (_pc_1235,_pc_3);;
          _T.bl = _bl_1237;
        }
        _T.r0_val = gensym201$$$const;
        _T.r0_lev = _lbl_1283;
        _T.r0_tlev = _lbl_1283;
        return _T.returnImmediate ();
      }
    }
  }
  this.$$$main$$$kont13.debugname = "$$$main$$$kont13"
  this.$$$main$$$kont14 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1445 = _STACK[ _SP + -36]
    const _$reg0_val_1443 = _STACK[ _SP + -32]
    const _$reg0_vlbl_1444 = _STACK[ _SP + -28]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1440 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1440);
    let _pc_1097 = _T.pc;
    let _bl_1166 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1441 = _T.r0_lev;
      const _$reg0_tlbl_1442 = _T.r0_tlev;
      const _pc_1096 = _T.pc;
      _pc_1097 = rt.raw_join (_pc_1096,_$reg0_vlbl_1441);;
      const _bl_1098 = _T.bl;
      const _bl_1099 = rt.raw_join (_bl_1098,_$reg0_vlbl_1441);;
      _bl_1166 = rt.raw_join (_bl_1099,_$reg0_tlbl_1442);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont13
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1440) {
      rt.rawAssertIsTuple (_$reg0_val_1443);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1443,0);
      const lval1109 = rt.raw_indexTuple(_$reg0_val_1443,0);
      const _val_1110 = lval1109.val;
      const _vlbl_1111 = lval1109.lev;
      const lval1127$val_opt = _val_1110 === gensym183$$$const;
      let _lbl_1135 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_1102 = rt.raw_join (_bl_1166,_$reg0_tlbl_1445);;
        const _bl_1106 = rt.raw_join (_bl_1102,_$reg0_vlbl_1444);;
        const _lbl_1115 = rt.raw_join (_pc_1097,_$reg0_vlbl_1444);;
        const _lbl_1116 = rt.raw_join (_lbl_1115,_vlbl_1111);;
        _lbl_1135 = rt.raw_join (_lbl_1116,_pc_3);;
        _T.bl = _bl_1106;
      }
      _T.r0_val = lval1127$val_opt;
      _T.r0_lev = _lbl_1135;
      _T.r0_tlev = _pc_1097;
      return _T.returnImmediate ();
    } else {
      let _lbl_1156 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_1156 = rt.raw_join (_pc_1097,_pc_3);;
        _T.bl = _bl_1166;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_1156;
      _T.r0_tlev = _lbl_1156;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont14.debugname = "$$$main$$$kont14"
  this.$$$main$$$kont15 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1443 = _T.r0_val;
    _STACK[ _SP + -32] =  _$reg0_val_1443
    const _raw_1023 = rt.raw_istuple(_$reg0_val_1443);
    _STACK[ _SP + -15] =  _raw_1023
    let _$reg0_vlbl_1444 = _T.pc;
    let _$reg0_tlbl_1445 = _T.pc;
    let _lbl_1026 = _T.pc;
    let _bl_1036 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      _$reg0_vlbl_1444 = _T.r0_lev;
      _$reg0_tlbl_1445 = _T.r0_tlev;
      const _pc_1024 = _T.pc;
      _lbl_1026 = rt.raw_join (_pc_1024,_$reg0_vlbl_1444);;
      const _bl_1035 = _T.bl;
      _bl_1036 = rt.raw_join (_bl_1035,_lbl_1026);;
    }
    _STACK[ _SP + -28] =  _$reg0_vlbl_1444
    _STACK[ _SP + -36] =  _$reg0_tlbl_1445
    _STACK[ _SP + -24] =  _lbl_1026
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont14
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_raw_1023) {
      rt.rawAssertIsTuple (_$reg0_val_1443);
      const _raw_1042 = rt.raw_tupleLength(_$reg0_val_1443);
      const lval1053$val_opt = _raw_1042 === gensym200$$$const;
      let _lbl_1061 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_1039 = rt.raw_join (_bl_1036,_$reg0_tlbl_1445);;
        _lbl_1061 = rt.raw_join (_lbl_1026,_pc_3);;
        _T.bl = _bl_1039;
      }
      _T.r0_val = lval1053$val_opt;
      _T.r0_lev = _lbl_1061;
      _T.r0_tlev = _lbl_1026;
      return _T.returnImmediate ();
    } else {
      let _lbl_1082 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_1082 = rt.raw_join (_lbl_1026,_pc_3);;
        _T.bl = _bl_1036;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_1082;
      _T.r0_tlev = _lbl_1082;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont15.debugname = "$$$main$$$kont15"
  this.$$$main$$$kont16 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1476 = _STACK[ _SP + -35]
    const _$reg0_val_1474 = _STACK[ _SP + -31]
    const _$reg0_vlbl_1475 = _STACK[ _SP + -27]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1457 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1457);
    let _bl_946 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1458 = _T.r0_lev;
      const _$reg0_tlbl_1459 = _T.r0_tlev;
      const _bl_941 = _T.bl;
      const _bl_942 = rt.raw_join (_bl_941,_$reg0_tlbl_1459);;
      _bl_946 = rt.raw_join (_bl_942,_$reg0_vlbl_1458);;
    }
    if (_$reg0_val_1457) {
      rt.rawAssertIsTuple (_$reg0_val_1474);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1474,1);
      const lval956 = rt.raw_indexTuple(_$reg0_val_1474,1);
      const _val_957 = lval956.val;
      const _vlbl_958 = lval956.lev;
      const _tlbl_959 = lval956.tlev;
      const _raw_972 = rt.loadLib("SimpleFileIO", "writeFile", this);
      _STACK[ _SP + -12] =  _raw_972
      const _raw_979 = rt.loadLib("module:6mcbrainqkmree4nrdu16nkmimg495d6mspbpbtk5dum1is8cfhg", "toText", this);
      rt.rawAssertIsFunction (_raw_979);
      let _pc_960 = _T.pc;
      let _lbl_963 = _T.pc;
      let _lbl_967 = _T.pc;
      let _bl_990 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_949 = rt.raw_join (_bl_946,_$reg0_tlbl_1476);;
        const _bl_953 = rt.raw_join (_bl_949,_$reg0_vlbl_1475);;
        _pc_960 = _T.pc;
        const _lbl_962 = rt.raw_join (_pc_960,_$reg0_vlbl_1475);;
        _lbl_963 = rt.raw_join (_lbl_962,_vlbl_958);;
        const _lbl_966 = rt.raw_join (_pc_960,_tlbl_959);;
        _lbl_967 = rt.raw_join (_lbl_966,_$reg0_vlbl_1475);;
        _bl_990 = rt.raw_join (_bl_953,_pc_960);;
        _T.bl = _bl_953;
      }
      _STACK[ _SP + -16] =  _pc_960
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont15
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont10
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _T.r0_val = _val_957;
      _T.r0_lev = _lbl_963;
      _T.r0_tlev = _lbl_967;
      if (! _STACK[ _SP +  -16] ) {
        _T.pc = _pc_960;
        _T.bl = _bl_990;
      }
      return _raw_979
    } else {
      if (! _STACK[ _SP +  -6] ) {
        const _bl_1448 = rt.raw_join (_bl_946,_pc_3);;
        const _pc_1451 = _T.pc;
        const _pc_1452 = rt.raw_join (_pc_1451,_pc_3);;
        _T.pc = _pc_1452;
        _T.bl = _bl_1448;
      }
      rt.rawErrorPos (gensym169$$$const,"trp-compiler/roundtrip.trp:14:18");
    }
  }
  this.$$$main$$$kont16.debugname = "$$$main$$$kont16"
  this.$$$main$$$kont17 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1476 = _STACK[ _SP + -35]
    const _$reg0_val_1474 = _STACK[ _SP + -31]
    const _$reg0_vlbl_1475 = _STACK[ _SP + -27]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1460 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1460);
    let _pc_868 = _T.pc;
    let _bl_937 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1461 = _T.r0_lev;
      const _$reg0_tlbl_1462 = _T.r0_tlev;
      const _pc_867 = _T.pc;
      _pc_868 = rt.raw_join (_pc_867,_$reg0_vlbl_1461);;
      const _bl_869 = _T.bl;
      const _bl_870 = rt.raw_join (_bl_869,_$reg0_vlbl_1461);;
      _bl_937 = rt.raw_join (_bl_870,_$reg0_tlbl_1462);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont16
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1460) {
      rt.rawAssertIsTuple (_$reg0_val_1474);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1474,0);
      const lval880 = rt.raw_indexTuple(_$reg0_val_1474,0);
      const _val_881 = lval880.val;
      const _vlbl_882 = lval880.lev;
      const lval898$val_opt = _val_881 === gensym172$$$const;
      let _lbl_906 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_873 = rt.raw_join (_bl_937,_$reg0_tlbl_1476);;
        const _bl_877 = rt.raw_join (_bl_873,_$reg0_vlbl_1475);;
        const _lbl_886 = rt.raw_join (_pc_868,_$reg0_vlbl_1475);;
        const _lbl_887 = rt.raw_join (_lbl_886,_vlbl_882);;
        _lbl_906 = rt.raw_join (_lbl_887,_pc_3);;
        _T.bl = _bl_877;
      }
      _T.r0_val = lval898$val_opt;
      _T.r0_lev = _lbl_906;
      _T.r0_tlev = _pc_868;
      return _T.returnImmediate ();
    } else {
      let _lbl_927 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_927 = rt.raw_join (_pc_868,_pc_3);;
        _T.bl = _bl_937;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_927;
      _T.r0_tlev = _lbl_927;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont17.debugname = "$$$main$$$kont17"
  this.$$$main$$$kont18 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1476 = _STACK[ _SP + -35]
    const _$reg0_val_1474 = _STACK[ _SP + -31]
    const _$reg0_vlbl_1475 = _STACK[ _SP + -27]
    const _lbl_596 = _STACK[ _SP + -18]
    const _pc_3 = _STACK[ _SP + -17]
    const _raw_593 = _STACK[ _SP + -13]
    const print5 = _STACK[ _SP + -7]
    const _$reg0_val_1468 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1468);
    let _pc_741 = _T.pc;
    let _bl_1465 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1469 = _T.r0_lev;
      const _$reg0_tlbl_1470 = _T.r0_tlev;
      const _pc_740 = _T.pc;
      _pc_741 = rt.raw_join (_pc_740,_$reg0_vlbl_1469);;
      const _bl_742 = _T.bl;
      const _bl_743 = rt.raw_join (_bl_742,_$reg0_vlbl_1469);;
      _bl_1465 = rt.raw_join (_bl_743,_$reg0_tlbl_1470);;
    }
    _T.setBranchFlag()
    if (_$reg0_val_1468) {
      rt.rawAssertIsTuple (_$reg0_val_1474);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1474,1);
      const lval753 = rt.raw_indexTuple(_$reg0_val_1474,1);
      const _val_754 = lval753.val;
      const _vlbl_755 = lval753.lev;
      const _tlbl_756 = lval753.tlev;
      rt.rawAssertIsString (_val_754);
      const _raw_779 = gensym108$$$const + _val_754;
      const _vlbl_790 = print5.lev;
      const _tlbl_795 = print5.tlev;
      const _val_798 = print5.val;
      rt.rawAssertIsFunction (_val_798);
      let _lbl_784 = _T.pc;
      let _pc_792 = _T.pc;
      let _bl_797 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_746 = rt.raw_join (_bl_1465,_$reg0_tlbl_1476);;
        const _bl_750 = rt.raw_join (_bl_746,_$reg0_vlbl_1475);;
        const _lbl_759 = rt.raw_join (_pc_741,_$reg0_vlbl_1475);;
        const _lbl_760 = rt.raw_join (_lbl_759,_vlbl_755);;
        const _lbl_763 = rt.raw_join (_pc_741,_tlbl_756);;
        const _lbl_764 = rt.raw_join (_lbl_763,_$reg0_vlbl_1475);;
        const _bl_771 = rt.raw_join (_bl_750,_pc_3);;
        const _bl_775 = rt.raw_join (_bl_771,_lbl_764);;
        const _lbl_783 = rt.raw_join (_pc_741,_pc_3);;
        _lbl_784 = rt.raw_join (_lbl_783,_lbl_760);;
        _pc_792 = rt.raw_join (_pc_741,_vlbl_790);;
        const _bl_794 = rt.raw_join (_bl_775,_vlbl_790);;
        _bl_797 = rt.raw_join (_bl_794,_tlbl_795);;
      }
      _T.r0_val = _raw_779;
      _T.r0_lev = _lbl_784;
      _T.r0_tlev = _pc_741;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_792;
        _T.bl = _bl_797;
      }
      return _val_798
    } else {
      let _pc_805 = _T.pc;
      let _bl_807 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _pc_805 = rt.raw_join (_pc_741,_lbl_596);;
        _bl_807 = rt.raw_join (_bl_1465,_lbl_596);;
        _T.pc = _pc_741;
        _T.bl = _bl_1465;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont17
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _T.setBranchFlag()
      if (_raw_593) {
        rt.rawAssertIsTuple (_$reg0_val_1474);
        const _raw_813 = rt.raw_tupleLength(_$reg0_val_1474);
        const lval824$val_opt = _raw_813 === gensym200$$$const;
        let _lbl_832 = _T.pc;
        if (! _STACK[ _SP +  -11] ) {
          const _bl_810 = rt.raw_join (_bl_807,_$reg0_tlbl_1476);;
          _lbl_832 = rt.raw_join (_pc_805,_pc_3);;
          _T.bl = _bl_810;
        }
        _T.r0_val = lval824$val_opt;
        _T.r0_lev = _lbl_832;
        _T.r0_tlev = _pc_805;
        return _T.returnImmediate ();
      } else {
        let _lbl_853 = _T.pc;
        if (! _STACK[ _SP +  -11] ) {
          _lbl_853 = rt.raw_join (_pc_805,_pc_3);;
          _T.bl = _bl_807;
        }
        _T.r0_val = gensym201$$$const;
        _T.r0_lev = _lbl_853;
        _T.r0_tlev = _lbl_853;
        return _T.returnImmediate ();
      }
    }
  }
  this.$$$main$$$kont18.debugname = "$$$main$$$kont18"
  this.$$$main$$$kont19 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1476 = _STACK[ _SP + -35]
    const _$reg0_val_1474 = _STACK[ _SP + -31]
    const _$reg0_vlbl_1475 = _STACK[ _SP + -27]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1471 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1471);
    let _pc_667 = _T.pc;
    let _bl_736 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1472 = _T.r0_lev;
      const _$reg0_tlbl_1473 = _T.r0_tlev;
      const _pc_666 = _T.pc;
      _pc_667 = rt.raw_join (_pc_666,_$reg0_vlbl_1472);;
      const _bl_668 = _T.bl;
      const _bl_669 = rt.raw_join (_bl_668,_$reg0_vlbl_1472);;
      _bl_736 = rt.raw_join (_bl_669,_$reg0_tlbl_1473);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont18
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1471) {
      rt.rawAssertIsTuple (_$reg0_val_1474);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1474,0);
      const lval679 = rt.raw_indexTuple(_$reg0_val_1474,0);
      const _val_680 = lval679.val;
      const _vlbl_681 = lval679.lev;
      const lval697$val_opt = _val_680 === gensym183$$$const;
      let _lbl_705 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_672 = rt.raw_join (_bl_736,_$reg0_tlbl_1476);;
        const _bl_676 = rt.raw_join (_bl_672,_$reg0_vlbl_1475);;
        const _lbl_685 = rt.raw_join (_pc_667,_$reg0_vlbl_1475);;
        const _lbl_686 = rt.raw_join (_lbl_685,_vlbl_681);;
        _lbl_705 = rt.raw_join (_lbl_686,_pc_3);;
        _T.bl = _bl_676;
      }
      _T.r0_val = lval697$val_opt;
      _T.r0_lev = _lbl_705;
      _T.r0_tlev = _pc_667;
      return _T.returnImmediate ();
    } else {
      let _lbl_726 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_726 = rt.raw_join (_pc_667,_pc_3);;
        _T.bl = _bl_736;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_726;
      _T.r0_tlev = _lbl_726;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont19.debugname = "$$$main$$$kont19"
  this.$$$main$$$kont20 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1474 = _T.r0_val;
    _STACK[ _SP + -31] =  _$reg0_val_1474
    const _raw_593 = rt.raw_istuple(_$reg0_val_1474);
    _STACK[ _SP + -13] =  _raw_593
    let _$reg0_vlbl_1475 = _T.pc;
    let _$reg0_tlbl_1476 = _T.pc;
    let _lbl_596 = _T.pc;
    let _bl_606 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      _$reg0_vlbl_1475 = _T.r0_lev;
      _$reg0_tlbl_1476 = _T.r0_tlev;
      const _pc_594 = _T.pc;
      _lbl_596 = rt.raw_join (_pc_594,_$reg0_vlbl_1475);;
      const _bl_605 = _T.bl;
      _bl_606 = rt.raw_join (_bl_605,_lbl_596);;
    }
    _STACK[ _SP + -27] =  _$reg0_vlbl_1475
    _STACK[ _SP + -35] =  _$reg0_tlbl_1476
    _STACK[ _SP + -18] =  _lbl_596
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont19
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_raw_593) {
      rt.rawAssertIsTuple (_$reg0_val_1474);
      const _raw_612 = rt.raw_tupleLength(_$reg0_val_1474);
      const lval623$val_opt = _raw_612 === gensym200$$$const;
      let _lbl_631 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_609 = rt.raw_join (_bl_606,_$reg0_tlbl_1476);;
        _lbl_631 = rt.raw_join (_lbl_596,_pc_3);;
        _T.bl = _bl_609;
      }
      _T.r0_val = lval623$val_opt;
      _T.r0_lev = _lbl_631;
      _T.r0_tlev = _lbl_596;
      return _T.returnImmediate ();
    } else {
      let _lbl_652 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_652 = rt.raw_join (_lbl_596,_pc_3);;
        _T.bl = _bl_606;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_652;
      _T.r0_tlev = _lbl_652;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont20.debugname = "$$$main$$$kont20"
  this.$$$main$$$kont21 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1507 = _STACK[ _SP + -34]
    const _$reg0_val_1505 = _STACK[ _SP + -30]
    const _$reg0_vlbl_1506 = _STACK[ _SP + -26]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1488 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1488);
    let _bl_546 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1489 = _T.r0_lev;
      const _$reg0_tlbl_1490 = _T.r0_tlev;
      const _bl_541 = _T.bl;
      const _bl_542 = rt.raw_join (_bl_541,_$reg0_tlbl_1490);;
      _bl_546 = rt.raw_join (_bl_542,_$reg0_vlbl_1489);;
    }
    if (_$reg0_val_1488) {
      rt.rawAssertIsTuple (_$reg0_val_1505);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1505,1);
      const lval556 = rt.raw_indexTuple(_$reg0_val_1505,1);
      const _val_557 = lval556.val;
      const _vlbl_558 = lval556.lev;
      const _tlbl_559 = lval556.tlev;
      const _raw_572 = rt.loadLib("module:6mcbrainqkmree4nrdu16nkmimg495d6mspbpbtk5dum1is8cfhg", "fromString", this);
      rt.rawAssertIsFunction (_raw_572);
      let _pc_560 = _T.pc;
      let _lbl_563 = _T.pc;
      let _lbl_567 = _T.pc;
      let _bl_583 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_549 = rt.raw_join (_bl_546,_$reg0_tlbl_1507);;
        const _bl_553 = rt.raw_join (_bl_549,_$reg0_vlbl_1506);;
        _pc_560 = _T.pc;
        const _lbl_562 = rt.raw_join (_pc_560,_$reg0_vlbl_1506);;
        _lbl_563 = rt.raw_join (_lbl_562,_vlbl_558);;
        const _lbl_566 = rt.raw_join (_pc_560,_tlbl_559);;
        _lbl_567 = rt.raw_join (_lbl_566,_$reg0_vlbl_1506);;
        _bl_583 = rt.raw_join (_bl_553,_pc_560);;
        _T.bl = _bl_553;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont20
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _T.r0_val = _val_557;
      _T.r0_lev = _lbl_563;
      _T.r0_tlev = _lbl_567;
      if (! _STACK[ _SP +  -11] ) {
        _T.pc = _pc_560;
        _T.bl = _bl_583;
      }
      return _raw_572
    } else {
      if (! _STACK[ _SP +  -6] ) {
        const _bl_1479 = rt.raw_join (_bl_546,_pc_3);;
        const _pc_1482 = _T.pc;
        const _pc_1483 = rt.raw_join (_pc_1482,_pc_3);;
        _T.pc = _pc_1483;
        _T.bl = _bl_1479;
      }
      rt.rawErrorPos (gensym169$$$const,"trp-compiler/roundtrip.trp:11:11");
    }
  }
  this.$$$main$$$kont21.debugname = "$$$main$$$kont21"
  this.$$$main$$$kont22 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1507 = _STACK[ _SP + -34]
    const _$reg0_val_1505 = _STACK[ _SP + -30]
    const _$reg0_vlbl_1506 = _STACK[ _SP + -26]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1491 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1491);
    let _pc_468 = _T.pc;
    let _bl_537 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1492 = _T.r0_lev;
      const _$reg0_tlbl_1493 = _T.r0_tlev;
      const _pc_467 = _T.pc;
      _pc_468 = rt.raw_join (_pc_467,_$reg0_vlbl_1492);;
      const _bl_469 = _T.bl;
      const _bl_470 = rt.raw_join (_bl_469,_$reg0_vlbl_1492);;
      _bl_537 = rt.raw_join (_bl_470,_$reg0_tlbl_1493);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont21
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1491) {
      rt.rawAssertIsTuple (_$reg0_val_1505);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1505,0);
      const lval480 = rt.raw_indexTuple(_$reg0_val_1505,0);
      const _val_481 = lval480.val;
      const _vlbl_482 = lval480.lev;
      const lval498$val_opt = _val_481 === gensym172$$$const;
      let _lbl_506 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_473 = rt.raw_join (_bl_537,_$reg0_tlbl_1507);;
        const _bl_477 = rt.raw_join (_bl_473,_$reg0_vlbl_1506);;
        const _lbl_486 = rt.raw_join (_pc_468,_$reg0_vlbl_1506);;
        const _lbl_487 = rt.raw_join (_lbl_486,_vlbl_482);;
        _lbl_506 = rt.raw_join (_lbl_487,_pc_3);;
        _T.bl = _bl_477;
      }
      _T.r0_val = lval498$val_opt;
      _T.r0_lev = _lbl_506;
      _T.r0_tlev = _pc_468;
      return _T.returnImmediate ();
    } else {
      let _lbl_527 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_527 = rt.raw_join (_pc_468,_pc_3);;
        _T.bl = _bl_537;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_527;
      _T.r0_tlev = _lbl_527;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont22.debugname = "$$$main$$$kont22"
  this.$$$main$$$kont23 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1507 = _STACK[ _SP + -34]
    const _$reg0_val_1505 = _STACK[ _SP + -30]
    const _$reg0_vlbl_1506 = _STACK[ _SP + -26]
    const _lbl_121 = _STACK[ _SP + -23]
    const _lbl_125 = _STACK[ _SP + -22]
    const _lbl_196 = _STACK[ _SP + -19]
    const _pc_3 = _STACK[ _SP + -17]
    const _raw_193 = _STACK[ _SP + -14]
    const _val_115 = _STACK[ _SP + -11]
    const print5 = _STACK[ _SP + -7]
    const _$reg0_val_1499 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1499);
    let _pc_341 = _T.pc;
    let _bl_1496 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1500 = _T.r0_lev;
      const _$reg0_tlbl_1501 = _T.r0_tlev;
      const _pc_340 = _T.pc;
      _pc_341 = rt.raw_join (_pc_340,_$reg0_vlbl_1500);;
      const _bl_342 = _T.bl;
      const _bl_343 = rt.raw_join (_bl_342,_$reg0_vlbl_1500);;
      _bl_1496 = rt.raw_join (_bl_343,_$reg0_tlbl_1501);;
    }
    _T.setBranchFlag()
    if (_$reg0_val_1499) {
      rt.rawAssertIsTuple (_$reg0_val_1505);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1505,1);
      rt.rawAssertIsString (_val_115);
      const _raw_379 = gensym105$$$const + _val_115;
      const _vlbl_390 = print5.lev;
      const _tlbl_395 = print5.tlev;
      const _val_398 = print5.val;
      rt.rawAssertIsFunction (_val_398);
      let _lbl_384 = _T.pc;
      let _pc_392 = _T.pc;
      let _bl_397 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_346 = rt.raw_join (_bl_1496,_$reg0_tlbl_1507);;
        const _bl_350 = rt.raw_join (_bl_346,_$reg0_vlbl_1506);;
        const _bl_371 = rt.raw_join (_bl_350,_pc_3);;
        const _bl_375 = rt.raw_join (_bl_371,_lbl_125);;
        const _lbl_383 = rt.raw_join (_pc_341,_pc_3);;
        _lbl_384 = rt.raw_join (_lbl_383,_lbl_121);;
        _pc_392 = rt.raw_join (_pc_341,_vlbl_390);;
        const _bl_394 = rt.raw_join (_bl_375,_vlbl_390);;
        _bl_397 = rt.raw_join (_bl_394,_tlbl_395);;
      }
      _T.r0_val = _raw_379;
      _T.r0_lev = _lbl_384;
      _T.r0_tlev = _pc_341;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_392;
        _T.bl = _bl_397;
      }
      return _val_398
    } else {
      let _pc_405 = _T.pc;
      let _bl_407 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _pc_405 = rt.raw_join (_pc_341,_lbl_196);;
        _bl_407 = rt.raw_join (_bl_1496,_lbl_196);;
        _T.pc = _pc_341;
        _T.bl = _bl_1496;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont22
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _T.setBranchFlag()
      if (_raw_193) {
        rt.rawAssertIsTuple (_$reg0_val_1505);
        const _raw_413 = rt.raw_tupleLength(_$reg0_val_1505);
        const lval424$val_opt = _raw_413 === gensym200$$$const;
        let _lbl_432 = _T.pc;
        if (! _STACK[ _SP +  -11] ) {
          const _bl_410 = rt.raw_join (_bl_407,_$reg0_tlbl_1507);;
          _lbl_432 = rt.raw_join (_pc_405,_pc_3);;
          _T.bl = _bl_410;
        }
        _T.r0_val = lval424$val_opt;
        _T.r0_lev = _lbl_432;
        _T.r0_tlev = _pc_405;
        return _T.returnImmediate ();
      } else {
        let _lbl_453 = _T.pc;
        if (! _STACK[ _SP +  -11] ) {
          _lbl_453 = rt.raw_join (_pc_405,_pc_3);;
          _T.bl = _bl_407;
        }
        _T.r0_val = gensym201$$$const;
        _T.r0_lev = _lbl_453;
        _T.r0_tlev = _lbl_453;
        return _T.returnImmediate ();
      }
    }
  }
  this.$$$main$$$kont23.debugname = "$$$main$$$kont23"
  this.$$$main$$$kont24 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1507 = _STACK[ _SP + -34]
    const _$reg0_val_1505 = _STACK[ _SP + -30]
    const _$reg0_vlbl_1506 = _STACK[ _SP + -26]
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1502 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1502);
    let _pc_267 = _T.pc;
    let _bl_336 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      const _$reg0_vlbl_1503 = _T.r0_lev;
      const _$reg0_tlbl_1504 = _T.r0_tlev;
      const _pc_266 = _T.pc;
      _pc_267 = rt.raw_join (_pc_266,_$reg0_vlbl_1503);;
      const _bl_268 = _T.bl;
      const _bl_269 = rt.raw_join (_bl_268,_$reg0_vlbl_1503);;
      _bl_336 = rt.raw_join (_bl_269,_$reg0_tlbl_1504);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont23
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1502) {
      rt.rawAssertIsTuple (_$reg0_val_1505);
      rt.rawAssertTupleLengthGreaterThan (_$reg0_val_1505,0);
      const lval279 = rt.raw_indexTuple(_$reg0_val_1505,0);
      const _val_280 = lval279.val;
      const _vlbl_281 = lval279.lev;
      const lval297$val_opt = _val_280 === gensym183$$$const;
      let _lbl_305 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_272 = rt.raw_join (_bl_336,_$reg0_tlbl_1507);;
        const _bl_276 = rt.raw_join (_bl_272,_$reg0_vlbl_1506);;
        const _lbl_285 = rt.raw_join (_pc_267,_$reg0_vlbl_1506);;
        const _lbl_286 = rt.raw_join (_lbl_285,_vlbl_281);;
        _lbl_305 = rt.raw_join (_lbl_286,_pc_3);;
        _T.bl = _bl_276;
      }
      _T.r0_val = lval297$val_opt;
      _T.r0_lev = _lbl_305;
      _T.r0_tlev = _pc_267;
      return _T.returnImmediate ();
    } else {
      let _lbl_326 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_326 = rt.raw_join (_pc_267,_pc_3);;
        _T.bl = _bl_336;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_326;
      _T.r0_tlev = _lbl_326;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont24.debugname = "$$$main$$$kont24"
  this.$$$main$$$kont25 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  -6
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _pc_3 = _STACK[ _SP + -17]
    const _$reg0_val_1505 = _T.r0_val;
    _STACK[ _SP + -30] =  _$reg0_val_1505
    const _raw_193 = rt.raw_istuple(_$reg0_val_1505);
    _STACK[ _SP + -14] =  _raw_193
    let _$reg0_vlbl_1506 = _T.pc;
    let _$reg0_tlbl_1507 = _T.pc;
    let _lbl_196 = _T.pc;
    let _bl_206 = _T.pc;
    if (! _STACK[ _SP +  -6] ) {
      _$reg0_vlbl_1506 = _T.r0_lev;
      _$reg0_tlbl_1507 = _T.r0_tlev;
      const _pc_194 = _T.pc;
      _lbl_196 = rt.raw_join (_pc_194,_$reg0_vlbl_1506);;
      const _bl_205 = _T.bl;
      _bl_206 = rt.raw_join (_bl_205,_lbl_196);;
    }
    _STACK[ _SP + -26] =  _$reg0_vlbl_1506
    _STACK[ _SP + -34] =  _$reg0_tlbl_1507
    _STACK[ _SP + -19] =  _lbl_196
    _SP_OLD = _SP; 
    _SP = _SP +  5 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont24
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_raw_193) {
      rt.rawAssertIsTuple (_$reg0_val_1505);
      const _raw_212 = rt.raw_tupleLength(_$reg0_val_1505);
      const lval223$val_opt = _raw_212 === gensym200$$$const;
      let _lbl_231 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        const _bl_209 = rt.raw_join (_bl_206,_$reg0_tlbl_1507);;
        _lbl_231 = rt.raw_join (_lbl_196,_pc_3);;
        _T.bl = _bl_209;
      }
      _T.r0_val = lval223$val_opt;
      _T.r0_lev = _lbl_231;
      _T.r0_tlev = _lbl_196;
      return _T.returnImmediate ();
    } else {
      let _lbl_252 = _T.pc;
      if (! _STACK[ _SP +  -11] ) {
        _lbl_252 = rt.raw_join (_lbl_196,_pc_3);;
        _T.bl = _bl_206;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_252;
      _T.r0_tlev = _lbl_252;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont25.debugname = "$$$main$$$kont25"
  this.$$$main$$$kont26 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  30
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_val_1537 = _T.r0_val;
    let _lbl_1529 = _T.pc;
    let _lbl_1532 = _T.pc;
    if (! _STACK[ _SP +  30] ) {
      const _$reg0_vlbl_1538 = _T.r0_lev;
      const _$reg0_tlbl_1539 = _T.r0_tlev;
      const _pc_1527 = _T.pc;
      _lbl_1529 = rt.raw_join (_pc_1527,_$reg0_vlbl_1538);;
      _lbl_1532 = rt.raw_join (_pc_1527,_$reg0_tlbl_1539);;
    }
    _T.r0_val = _$reg0_val_1537;
    _T.r0_lev = _lbl_1529;
    _T.r0_tlev = _lbl_1532;
    return _T.returnImmediate ();
  }
  this.$$$main$$$kont26.debugname = "$$$main$$$kont26"
  this.$$$main$$$kont27 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  30
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _$reg0_tlbl_1545 = _STACK[ _SP + 3]
    const _$reg0_val_1543 = _STACK[ _SP + 7]
    const _$reg0_vlbl_1544 = _STACK[ _SP + 11]
    const _pc_3 = _STACK[ _SP + 19]
    const gensym204 = _STACK[ _SP + 28]
    const print5 = _STACK[ _SP + 29]
    const _$reg0_val_1540 = _T.r0_val;
    rt.rawAssertIsBoolean (_$reg0_val_1540);
    let _pc_106 = _T.pc;
    let _bl_1523 = _T.pc;
    if (! _STACK[ _SP +  30] ) {
      const _$reg0_vlbl_1541 = _T.r0_lev;
      const _$reg0_tlbl_1542 = _T.r0_tlev;
      const _pc_105 = _T.pc;
      _pc_106 = rt.raw_join (_pc_105,_$reg0_vlbl_1541);;
      const _bl_107 = _T.bl;
      const _bl_108 = rt.raw_join (_bl_107,_$reg0_vlbl_1541);;
      _bl_1523 = rt.raw_join (_bl_108,_$reg0_tlbl_1542);;
    }
    _SP_OLD = _SP; 
    _SP = _SP +  36 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont26
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_$reg0_val_1540) {
      rt.rawAssertIsList (_$reg0_val_1543);
      const lval114 = rt.head(_$reg0_val_1543);
      const _val_115 = lval114.val;
      _STACK[ _SP + -11] =  _val_115
      const _vlbl_116 = lval114.lev;
      const _tlbl_117 = lval114.tlev;
      let _bl_111 = _T.pc;
      let _lbl_120 = _T.pc;
      let _lbl_121 = _T.pc;
      let _lbl_125 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _bl_111 = rt.raw_join (_bl_1523,_$reg0_tlbl_1545);;
        _lbl_120 = rt.raw_join (_pc_106,_$reg0_vlbl_1544);;
        _lbl_121 = rt.raw_join (_lbl_120,_vlbl_116);;
        const _lbl_124 = rt.raw_join (_pc_106,_tlbl_117);;
        _lbl_125 = rt.raw_join (_lbl_124,_$reg0_vlbl_1544);;
      }
      _STACK[ _SP + -23] =  _lbl_121
      _STACK[ _SP + -22] =  _lbl_125
      const gensym195 = rt.constructLVal (_val_115,_lbl_121,_lbl_125);
      const _raw_135 = rt.tail(_$reg0_val_1543);
      rt.rawAssertIsList (_raw_135);
      const lval149 = rt.head(_raw_135);
      const _val_150 = lval149.val;
      _STACK[ _SP + -10] =  _val_150
      const _vlbl_151 = lval149.lev;
      const _tlbl_152 = lval149.tlev;
      let _bl_146 = _T.pc;
      let _lbl_156 = _T.pc;
      let _lbl_160 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _bl_146 = rt.raw_join (_bl_111,_pc_106);;
        _lbl_156 = rt.raw_join (_lbl_120,_vlbl_151);;
        const _lbl_159 = rt.raw_join (_pc_106,_tlbl_152);;
        _lbl_160 = rt.raw_join (_lbl_159,_lbl_120);;
      }
      _STACK[ _SP + -21] =  _lbl_156
      _STACK[ _SP + -20] =  _lbl_160
      const gensym193 = rt.constructLVal (_val_150,_lbl_156,_lbl_160);
      _STACK[ _SP + -9] =  gensym193
      const _raw_165 = rt.loadLib("SimpleFileIO", "readFile", this);
      const _raw_172 = rt.mkTuple([gensym204, gensym195], false);
      rt.rawAssertIsFunction (_raw_165);
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_106;
        _T.bl = _bl_146;
      }
      _SP_OLD = _SP; 
      _SP = _SP +  5 ;
      _STACK[_SP - 5] = _SP_OLD;
      _STACK[_SP - 4] = _T.pc;
      _STACK[_SP - 3] = this.$$$main$$$kont25
      _STACK[_SP - 2] = _T.mailbox.mclear;
      _STACK[_SP - 1] = false;
      _T._sp = _SP;
      _T.r0_val = _raw_172;
      _T.r0_lev = _pc_106;
      _T.r0_tlev = _pc_106;
      if (! _STACK[ _SP +  -11] ) {
        _T.pc = _pc_106;
        _T.bl = _bl_146;
      }
      return _raw_165
    } else {
      const _vlbl_1508 = print5.lev;
      const _tlbl_1513 = print5.tlev;
      const _val_1516 = print5.val;
      rt.rawAssertIsFunction (_val_1516);
      let _pc_1510 = _T.pc;
      let _bl_1515 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _pc_1510 = rt.raw_join (_pc_106,_vlbl_1508);;
        const _bl_1512 = rt.raw_join (_bl_1523,_vlbl_1508);;
        _bl_1515 = rt.raw_join (_bl_1512,_tlbl_1513);;
      }
      _T.r0_val = gensym196$$$const;
      _T.r0_lev = _pc_3;
      _T.r0_tlev = _pc_3;
      if (! _STACK[ _SP +  -6] ) {
        _T.pc = _pc_1510;
        _T.bl = _bl_1515;
      }
      return _val_1516
    }
  }
  this.$$$main$$$kont27.debugname = "$$$main$$$kont27"
  this.$$$main$$$kont28 = () => {
    let _T = rt.runtime.$t
    _T.currentSourceMap = this.__isDynamic ? { ...(this.__sourceMap || {}), __isDynamic: true } : this.__sourceMap
    let _STACK = _T.callStack
    let _SP = _T._sp
    let _SP_OLD
    _T.sparseSlot = _SP +  30
    _T.updateSparseBitOnReturn()
    const gensym200$$$const = 2
    const gensym108$$$const = "DECODE FAILED: "
    const gensym113$$$const = "OK"
    const gensym105$$$const = "cannot read "
    const gensym111$$$const = "cannot write "
    const gensym183$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#ERR"
    const gensym172$$$const = "k3gbkho4cpq7ogmcjd296965sdjlmc59argsdahlfhvq1d65bb8g#outcome#OK"
    const gensym169$$$const = "pattern match failure in case expression"
    const gensym196$$$const = "usage: roundtrip <in.sexp> <out.sexp>"
    const gensym201$$$const = false
    const _pc_3 = _STACK[ _SP + 19]
    const _$reg0_val_1543 = _T.r0_val;
    _STACK[ _SP + 7] =  _$reg0_val_1543
    const _raw_32 = rt.raw_islist(_$reg0_val_1543);
    let _$reg0_vlbl_1544 = _T.pc;
    let _$reg0_tlbl_1545 = _T.pc;
    let _lbl_35 = _T.pc;
    let _bl_45 = _T.pc;
    if (! _STACK[ _SP +  30] ) {
      _$reg0_vlbl_1544 = _T.r0_lev;
      _$reg0_tlbl_1545 = _T.r0_tlev;
      const _pc_33 = _T.pc;
      _lbl_35 = rt.raw_join (_pc_33,_$reg0_vlbl_1544);;
      const _bl_44 = _T.bl;
      _bl_45 = rt.raw_join (_bl_44,_lbl_35);;
    }
    _STACK[ _SP + 11] =  _$reg0_vlbl_1544
    _STACK[ _SP + 3] =  _$reg0_tlbl_1545
    _SP_OLD = _SP; 
    _SP = _SP +  36 ;
    _STACK[_SP - 5] = _SP_OLD;
    _STACK[_SP - 4] = _T.pc;
    _STACK[_SP - 3] = this.$$$main$$$kont27
    _STACK[_SP - 2] = _T.mailbox.mclear;
    _STACK[_SP - 1] = false;
    _T._sp = _SP;
    _T.setBranchFlag()
    if (_raw_32) {
      rt.rawAssertIsList (_$reg0_val_1543);
      const _raw_51 = rt.raw_listLength(_$reg0_val_1543);
      const lval62$val_opt = _raw_51 === gensym200$$$const;
      let _lbl_70 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        const _bl_48 = rt.raw_join (_bl_45,_$reg0_tlbl_1545);;
        _lbl_70 = rt.raw_join (_lbl_35,_pc_3);;
        _T.bl = _bl_48;
      }
      _T.r0_val = lval62$val_opt;
      _T.r0_lev = _lbl_70;
      _T.r0_tlev = _lbl_35;
      return _T.returnImmediate ();
    } else {
      let _lbl_91 = _T.pc;
      if (! _STACK[ _SP +  -6] ) {
        _lbl_91 = rt.raw_join (_lbl_35,_pc_3);;
        _T.bl = _bl_45;
      }
      _T.r0_val = gensym201$$$const;
      _T.r0_lev = _lbl_91;
      _T.r0_tlev = _lbl_91;
      return _T.returnImmediate ();
    }
  }
  this.$$$main$$$kont28.debugname = "$$$main$$$kont28"
}
module.exports = Top
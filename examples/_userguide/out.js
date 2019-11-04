function Top (rt) {
  this.uuid = rt.rt_uuid
  this.libSet = new Set ()
  this.libs = []
  this.addLib = function (lib, decl) { if (!this.libSet.has (lib +'.'+decl)) { this.libSet.add (lib +'.'+decl); this.libs.push ({lib:lib, decl:decl})} }
  this.loadlibs = function (cb) { rt.linkLibs (this.libs, this, cb) }
  this.serializedatoms = "AgAAAAAAAAAA"
  this.main = function ($$env,$$authorityarg) {
    const gensym2 = $$authorityarg;
    const gensym1 = rt.mkVal(42);
    rt.ret (gensym1);
  }
  this.main.deps = [];
  this.main.serialized = "AQAAAAAAAAAEbWFpbgAAAAAAAAAFJCRlbnYAAAAAAAAADiQkYXV0aG9yaXR5YXJnAAAAAAAAAAIAAAAAAAAAAAdnZW5zeW0yBgAAAAAAAAAOJCRhdXRob3JpdHlhcmcAAAAAAAAAAAdnZW5zeW0xBQAAAAAAKgEAAAAAAAAAAAdnZW5zeW0x";
}
module.exports = Top 
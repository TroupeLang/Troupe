

function isListFlagSet (x) {

  return (x.isList == true )
  /*
  console.log (x)
  if (x) return (x==true);
  return false;
  */
}


function isTupleFlagSet (x) {
  // return true;
  return (x.isTuple == true)
}


module.exports = {
    isListFlagSet : isListFlagSet,
    isTupleFlagSet : isTupleFlagSet
}
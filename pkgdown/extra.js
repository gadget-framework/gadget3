window.addEventListener("DOMContentLoaded", (event) => {

  // https://stackoverflow.com/a/9521582
  MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
    var TEX = MathJax.InputJax.TeX;
    var PREFILTER = TEX.prefilterMath;
    TEX.Augment({
      prefilterMath: function (math,displaymode,script) {
        if (displaymode) {math = "\\LARGE{"+math+"}"}
        return PREFILTER.call(TEX,math,displaymode,script);
      }
    });
  });

});

Cable.define({
  $:Cable.library("jquery.min.js"),
  marked:Cable.library("marked.js"),

  source:Cable.textbox("input"),
  dest:function($, marked, source) {
    $("output").html(marked(source()));
  }
});

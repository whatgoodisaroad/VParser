Cable.define({
  dest:function($, marked, source) {
    $("#output").html(marked(source()));
  }
});

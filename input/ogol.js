function render() {
  var c = document.getElementById("myCanvas");
  var ctx = c.getContext("2d");
  ctx.beginPath();
  ctx.moveTo(250, 250);
  ctx.lineTo(350, 250);
  ctx.moveTo(350, 250);
  ctx.lineTo(350, 150);
  ctx.moveTo(350, 150);
  ctx.lineTo(250, 150);
  ctx.moveTo(250, 150);
  ctx.lineTo(250, 250);
  ctx.stroke();
}
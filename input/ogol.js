function render() {
  var c = document.getElementById("myCanvas");
  var ctx = c.getContext("2d");
  ctx.beginPath();
  ctx.moveTo(300, 250);
  ctx.lineTo(350, 250);
  ctx.moveTo(350, 250);
  ctx.lineTo(350, 200);
  ctx.moveTo(350, 200);
  ctx.lineTo(300, 200);
  ctx.moveTo(300, 200);
  ctx.lineTo(300, 250);
  ctx.moveTo(300, 275);
  ctx.lineTo(300, 325);
  ctx.moveTo(300, 325);
  ctx.lineTo(350, 325);
  ctx.moveTo(350, 325);
  ctx.lineTo(350, 275);
  ctx.moveTo(350, 275);
  ctx.lineTo(300, 275);
  ctx.moveTo(275, 275);
  ctx.lineTo(225, 275);
  ctx.moveTo(225, 275);
  ctx.lineTo(225, 325);
  ctx.moveTo(225, 325);
  ctx.lineTo(275, 325);
  ctx.moveTo(275, 325);
  ctx.lineTo(275, 275);
  ctx.moveTo(275, 250);
  ctx.lineTo(275, 200);
  ctx.moveTo(275, 200);
  ctx.lineTo(225, 200);
  ctx.moveTo(225, 200);
  ctx.lineTo(225, 250);
  ctx.moveTo(225, 250);
  ctx.lineTo(275, 250);
  ctx.stroke();
}
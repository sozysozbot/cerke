var img_board = new Image();
img_board.src = "cert_kaik.png";

/*var img_piece = {};

[
  "船",
  "無",
  "兵",
  "弓",
  "車",
  "虎",
  "馬",
  "筆",
  "巫",
  "将",
  "王",
  "皇"
].forEach(function(k) {
  img_piece[k] = new Image();
  img_piece[k].src = "pieces/" + k + ".png";
});*/

var img_contiguous = {};
img_contiguous.normal = new Image();
img_contiguous.normal.src = "駒画像/通常駒.png";
img_contiguous.blackwhite = new Image();
img_contiguous.blackwhite.src = "駒画像/白黒.png";
img_contiguous.cursive = new Image();
img_contiguous.cursive.src = "駒画像/略字駒.png";
img_contiguous.cursive_blackwhite = new Image();
img_contiguous.cursive_blackwhite.src = "駒画像/略字白黒.png";

var ctx = null;

//GUI
function getHeight() {
  return +document.getElementById("height").value;
}

function getPaddings() {
  var height = getHeight();
  // top, right, bottom, left
  return [50, 10, 50, 10].map(function(x) {
    return (x * height) / 400;
  });
}

function init() {
  ctx = document.getElementById("cv").getContext("2d");
}

function draw1() {
  var height = getHeight();
  var pads = getPaddings();
  var full_w = height + pads[1] + pads[3];
  var full_h = height + pads[0] + pads[2];

  document.getElementById("cv").width = "" + full_w;
  document.getElementById("cv").height = "" + full_h;

  if (!document.getElementById("transparent").checked) {
    ctx.fillStyle = "white";
    ctx.fillRect(0, 0, full_w, full_h);
  }

  ctx.drawImage(img_board, pads[3], pads[0], height, height);
}

function drawPiece(x, y, txt, isRed, isReversed) {
  /* drawPieceByImg(x, y, txt, isRed, isReversed); */
  var type;
  ["normal", "blackwhite", "cursive", "cursive_blackwhite"].forEach(function(
    t
  ) {
    if (document.getElementById("radio_" + t).checked) {
      type = t;
    }
  });

  if (type) {
    drawPieceByContiguousImg(x, y, txt, isRed, isReversed, type);
  } else {
    /* fallback */
    drawPieceByChar(x, y, txt, isRed, isReversed);
  }
}

function drawPieceByContiguousImg(x, y, txt, isRed, isReversed, type) {
  var height = getHeight();
  var pads = getPaddings();
  var X_ = pads[3] + (x * height) / 9;
  var Y_ = pads[0] + (y * height) / 9;
  var size = Math.round(height * 0.11);

  var img_num = {
    船: 0,
    無: 1,
    兵: 2,
    弓: 3,
    車: 4,
    虎: 5,
    馬: 6,
    筆: 7,
    巫: 8,
    将: 9,
    王: 10,
    皇: 11
  }[txt];

  if (isReversed) {
    ctx.rotate(Math.PI);
    ctx.drawImage(
      img_contiguous[type],
      isRed ? 256 : 0,
      img_num * 256,
      256,
      256,
      -X_ - size,
      -Y_ - size,
      size,
      size
    );
    ctx.rotate(Math.PI);
  } else {
    ctx.drawImage(
      img_contiguous[type],
      isRed ? 256 : 0,
      img_num * 256,
      256,
      256,
      X_,
      Y_,
      size,
      size
    );
  }
}

/*
function drawPieceByImg(x, y, txt, isRed, isReversed) {
  var height = getHeight();
  var pads = getPaddings();
  var X_ = pads[3] + (x * height) / 9;
  var Y_ = pads[0] + (y * height) / 9;
  var size = Math.round(height * 0.11);

  if (isReversed) {
    ctx.rotate(Math.PI);
    ctx.drawImage(img_piece[txt], -X_ - size, -Y_ - size, size, size);
    ctx.rotate(Math.PI);
  } else {
    ctx.drawImage(img_piece[txt], X_, Y_, size, size);
  }
}
*/

function drawPieceByChar(x, y, txt, isRed, isReversed) {
  var height = getHeight();

  ctx.font = Math.round(height * 0.085) + "px PekTak";
  ctx.fillStyle = isRed ? "red" : "black";
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";

  var pads = getPaddings();

  var X = pads[3] + ((x + 1 / 2) * height) / 9;
  var Y = pads[0] + ((y + 1 / 2) * height) / 9;

  if (isReversed) {
    ctx.rotate(Math.PI);
    ctx.fillText(txt, -X, -Y);
    ctx.rotate(Math.PI);
  } else {
    ctx.fillText(txt, X, Y);
  }
}

function error(txt) {
  alert(txt);
}

var conv_obj = {
  "!": "船",
  "0": "無",
  "1": "兵",
  "2": "弓",
  "3": "車",
  "4": "虎",
  "5": "馬",
  "6": "筆",
  "7": "巫",
  "8": "将",
  "#": "王",
  $: "皇"
};

function load_board() {
  draw1();
  var info = document.getElementById("load_board").value;
  var res_arr = parser(info);
  if (res_arr.length !== 81) {
    error(
      "Parse error: Expected 81 elements, but got " +
        res_arr.length +
        " element(s)"
    );
    return null;
  }

  for (var i = 0; i < 81; i++) {
    if (res_arr[i]) {
      drawPiece(
        i % 9,
        (i / 9) | 0,
        res_arr[i].txt,
        res_arr[i].isRed,
        res_arr[i].isReversed
      );
    }
  }
}

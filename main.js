var img = new Image();
 	img.src = "cert_kaik.png"; 

var ctx = null;

//GUI
function getHeight()
{
	return +document.getElementById('height').value;
}

function getPaddings()
{
	var height = getHeight();
	// top, right, bottom, left
	return [50,10,50,10].map(function(x){return x*height/400;});
}

function init()
{
	ctx = document.getElementById("cv").getContext("2d");
}

function draw1(){
	var height = getHeight();
	var pads = getPaddings();
	var full_w = (height + pads[1] + pads[3])
	var full_h = (height + pads[0] + pads[2])

	document.getElementById("cv").width = "" + full_w;
	document.getElementById("cv").height = "" + full_h;

	if(!document.getElementById('transparent').checked) {
		ctx.fillStyle = "white";
		ctx.fillRect(0, 0, full_w, full_h);
	}

	ctx.drawImage(img,pads[3],pads[0],height,height);
}

function drawChar(x, y, txt, isRed, isReversed)
{
	var height = getHeight();

	ctx.font = Math.round(height * 0.085)+"px PekTak"; 
	ctx.fillStyle = isRed ? "red" : "black";
	ctx.textAlign = "center";
	ctx.textBaseline = "middle";

	var pads = getPaddings();

	var X = pads[3] + (x + 1/2) * height / 9;
	var Y = pads[0] + (y + 1/2) * height / 9;

	if(isReversed){
		ctx.rotate(Math.PI);
		ctx.fillText(txt, -X, -Y);
		ctx.rotate(Math.PI);
	} else {
		ctx.fillText(txt, X, Y);
	}
}

function error(txt){alert(txt)}

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
	"$": "皇"
}


function load_board()
{
	draw1();
	var info = document.getElementById("load_board").value;
	var res_arr = parser(info);
	if(res_arr.length !== 81) {
		error("Parse error: Expected 81 elements, but got " + res_arr.length + " element(s)");
		return null;
	}

	for(var i=0; i<81; i++){
		if(res_arr[i]) {
			drawChar(i%9, (i/9)|0, res_arr[i].txt, res_arr[i].isRed, res_arr[i].isReversed);
		}
	}

}








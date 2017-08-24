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
	// top, right, bottom, left
	return [0,100,0,100];
}

function init()
{
	ctx = document.getElementById("cv").getContext("2d");
}

function draw1(){
	var height = getHeight();
	var pads = getPaddings()
	document.getElementById("cv").width = "" + (height + pads[1] + pads[3]);
	document.getElementById("cv").height = "" + (height + pads[0] + pads[2]);
	ctx.drawImage(img,pads[3],pads[0],height,height);
}

function drawChar(x, y, txt, isRed, isReversed)
{

	ctx.font = "40px PekTak"; 
	ctx.fillStyle = isRed ? "red" : "black";
	ctx.textAlign = "center";
	ctx.textBaseline = "middle";

	var height = getHeight();
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

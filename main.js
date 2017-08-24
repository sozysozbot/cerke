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

"use strict";
function openClose() {
    var men = document.getElementById("menu");
    var but = document.getElementById("menuButton");
    men.style.left = (men.style.left == "0px") ? "-450px" : "0px";
    but.innerText = (but.innerText == "≡") ? "×" : "≡";
    but.style.left = (men.style.left == "0px") ? "450px" : "10px";
}
var hs = document.querySelectorAll("h2,h3,h4,h5,h6");
var length = hs.length;
var menu = "";
var id = 0;
function anchor(element) { return '<a href="#' + element.id + '">' + element.innerText + '</a><br>'; }
for (var i = 0; i < length; i++) {
    if (hs[i].classList.contains("do_not_create_header")) {
        continue;
    }
    if (!hs[i].id) {
        hs[i].id = "id" + id++;
    }
    switch (hs[i].tagName) {
        case "H2":
            menu += anchor(hs[i]);
            break;
        case "H3":
            menu += '<div class="indent">' + anchor(hs[i]) + "</div>";
            break;
        case "H4":
            menu += '<div class="double-indent">' + anchor(hs[i]) + "</div>";
            break;
        case "H5":
            menu += '<div class="triple-indent">' + anchor(hs[i]) + "</div>";
            break;
        case "H6":
            menu += '<div class="quadruple-indent">' + anchor(hs[i]) + "</div>";
            break;
    }
}
document.getElementById("menu").innerHTML = menu;

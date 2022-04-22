"use strict";

exports.replaceContent = htmlElement => () => {
  fetch("./README.html", {cache: "no-store"}).then(response => response.text()).then(text => htmlElement.innerHTML = text);
}
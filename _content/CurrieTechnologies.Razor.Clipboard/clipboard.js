/******/ (() => { // webpackBootstrap
var __webpack_exports__ = {};
const namespace = "CurrieTechnologies.Razor.Clipboard";

async function dispatchReadResponse(id, text) {
  await DotNet.invokeMethodAsync(namespace, "ReceiveReadResponse", id, text);
}

async function dispatchWriteResponse(id) {
  await DotNet.invokeMethodAsync(namespace, "ReceiveWriteResponse", id);
}

window["CurrieTechnologies"] = window["CurrieTechnologies"] || {};
window["CurrieTechnologies"]["Razor"] = window["CurrieTechnologies"]["Razor"] || {};
window["CurrieTechnologies"]["Razor"]["Clipboard"] = window["CurrieTechnologies"]["Razor"]["Clipboard"] || {};

window["CurrieTechnologies"]["Razor"]["Clipboard"]["ReadText"] = async requestId => {
  const text = await window.navigator.clipboard.readText();
  await dispatchReadResponse(requestId, text);
};

window["CurrieTechnologies"]["Razor"]["Clipboard"]["WriteText"] = async (requestId, textToWrite) => {
  await window.navigator.clipboard.writeText(textToWrite);
  await dispatchWriteResponse(requestId);
};

window["CurrieTechnologies"]["Razor"]["Clipboard"]["IsSupported"] = () => !!window.navigator.clipboard && !!window.navigator.clipboard.writeText && !!window.navigator.clipboard.readText;
/******/ })()
;
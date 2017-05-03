"use strict";

var IDOM = require("incremental-dom");

exports.foreignNull = null;

exports.elementOpenImpl = function (tagname, key, staticProperties, properties) {
  return function () {
    return IDOM.elementOpen.apply(
        null,
        [tagname, key, staticProperties].concat(properties));
  };
};
exports.elementOpenStartImpl = function (tagname, key, staticProperties) {
  return function () {
    IDOM.elementOpenStart(tagname, key, staticProperties);
  };
};
exports.attr = function (name) {
  return function (value) {
    return function () {
      IDOM.attr(name, value);
    };
  };
};
exports.elementOpenEnd = function () {
  return IDOM.elementOpenEnd();
};
exports.elementClose = function (tagname) {
  return function () {
    return IDOM.elementClose(tagname);
  };
};
exports.elementVoidImpl = function (tagname, key, staticProperties, properties) {
  return function () {
    return IDOM.elementVoid.apply(
      null,
      [tagname, key, staticProperties].concat(properties));
  };
};
exports.text = function (text) {
  return function () {
    return IDOM.text(text);
  };
};
exports.patch = function (htmlElement) {
  return function (description) {
    return function () {
      IDOM.patch(htmlElement, description);
    };
  };
};
exports.currentElement = function () {
  return IDOM.currentElement();
};
exports.currentPointerImpl = function () {
  return IDOM.currentPointer();
};
exports.skip = function () {
  IDOM.skip();
};
exports.skipNode = function () {
  IDOM.skipNode();
};

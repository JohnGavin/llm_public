/* Minimal hover popup for span.tt elements (llmpublic::tt()).
 * Shows the data-tippy-content attribute as HTML in a popup on mouse
 * hover and on keyboard focus; Escape closes it. No external library. */
(function () {
  "use strict";
  var popup = null;
  var owner = null;

  function hide() {
    if (popup && popup.parentNode) {
      popup.parentNode.removeChild(popup);
    }
    popup = null;
    owner = null;
  }

  function show(el) {
    hide();
    var html = el.getAttribute("data-tippy-content");
    if (!html) {
      return;
    }
    popup = document.createElement("div");
    popup.className = "tt-popup";
    popup.setAttribute("role", "tooltip");
    popup.innerHTML = html;
    document.body.appendChild(popup);
    owner = el;
    var rect = el.getBoundingClientRect();
    var top = rect.bottom + window.scrollY + 6;
    var left = Math.max(
      8,
      Math.min(
        rect.left + window.scrollX,
        window.scrollX + document.documentElement.clientWidth - popup.offsetWidth - 8
      )
    );
    popup.style.top = top + "px";
    popup.style.left = left + "px";
  }

  function target(event) {
    var node = event.target;
    return node && node.closest ? node.closest("span.tt") : null;
  }

  document.addEventListener("mouseover", function (event) {
    var el = target(event);
    if (el) {
      show(el);
    } else if (popup && !(event.target.closest && event.target.closest(".tt-popup"))) {
      hide();
    }
  });
  document.addEventListener("focusin", function (event) {
    var el = target(event);
    if (el) {
      show(el);
    }
  });
  document.addEventListener("focusout", function (event) {
    if (target(event) === owner) {
      hide();
    }
  });
  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
      hide();
    }
  });
})();

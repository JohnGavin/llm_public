/* Minimal hover popup for span.tt elements (llmpublic::tt()).
 * Shows the data-tippy-content attribute as HTML in a popup on mouse
 * hover and on keyboard focus. The popup stays open while the pointer
 * travels from the term to the popup (short hide delay), so links inside
 * it can be clicked. From a focused term, Tab moves focus into the popup's
 * first link; Escape closes the popup and returns focus to the term.
 * No external library. */
(function () {
  "use strict";
  var HIDE_DELAY_MS = 200;
  var popup = null;
  var owner = null;
  var hideTimer = null;
  var counter = 0;

  function cancelHide() {
    if (hideTimer !== null) {
      window.clearTimeout(hideTimer);
      hideTimer = null;
    }
  }

  function hide() {
    cancelHide();
    if (owner) {
      owner.removeAttribute("aria-describedby");
    }
    if (popup && popup.parentNode) {
      popup.parentNode.removeChild(popup);
    }
    popup = null;
    owner = null;
  }

  function scheduleHide() {
    if (!popup || hideTimer !== null) {
      return;
    }
    hideTimer = window.setTimeout(hide, HIDE_DELAY_MS);
  }

  function inPopup(node) {
    return !!(popup && node && popup.contains(node));
  }

  function inOwner(node) {
    return !!(owner && node && owner.contains(node));
  }

  function links() {
    return popup ? Array.prototype.slice.call(popup.querySelectorAll("a[href]")) : [];
  }

  function show(el) {
    cancelHide();
    if (el === owner && popup) {
      return;
    }
    hide();
    var html = el.getAttribute("data-tippy-content");
    if (!html) {
      return;
    }
    counter += 1;
    popup = document.createElement("div");
    popup.className = "tt-popup";
    popup.id = "tt-popup-" + counter;
    popup.setAttribute("role", "tooltip");
    popup.innerHTML = html;
    document.body.appendChild(popup);
    owner = el;
    owner.setAttribute("aria-describedby", popup.id);
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
    } else if (inPopup(event.target)) {
      cancelHide();
    } else if (popup) {
      scheduleHide();
    }
  });
  document.addEventListener("mouseout", function (event) {
    // Pointer left the browser window entirely.
    if (!event.relatedTarget) {
      scheduleHide();
    }
  });
  document.addEventListener("focusin", function (event) {
    var el = target(event);
    if (el) {
      show(el);
    } else if (inPopup(event.target)) {
      cancelHide();
    } else if (popup) {
      hide();
    }
  });
  document.addEventListener("focusout", function (event) {
    var next = event.relatedTarget;
    if (!popup || inPopup(next) || inOwner(next)) {
      return;
    }
    if (inOwner(event.target) || inPopup(event.target)) {
      hide();
    }
  });
  document.addEventListener("keydown", function (event) {
    if (!popup) {
      return;
    }
    var active = document.activeElement;
    if (event.key === "Escape") {
      var wasInside = inPopup(active);
      var term = owner;
      // Move focus out of the popup first: removing a focused node would
      // drop focus to <body> after our handler returns.
      if (wasInside && term) {
        term.focus();
      }
      hide();
      return;
    }
    if (event.key !== "Tab") {
      return;
    }
    var items = links();
    if (!items.length) {
      return;
    }
    if (active === owner && !event.shiftKey) {
      event.preventDefault();
      items[0].focus();
    } else if (inPopup(active) && event.shiftKey && active === items[0]) {
      event.preventDefault();
      owner.focus();
    } else if (inPopup(active) && !event.shiftKey && active === items[items.length - 1]) {
      // Hand focus back to the term so the default Tab continues from it.
      owner.focus();
    }
  });
})();

(function () {
  "use strict";

  var enterBound = false;
  var autoScrollBound = false;

  function scrollChatToBottom(smooth) {
    var el = document.getElementById("chat_thread_container");
    if (!el) return;
    if (typeof el.scrollTo === "function") {
      el.scrollTo({ top: el.scrollHeight, behavior: smooth ? "smooth" : "auto" });
    } else {
      el.scrollTop = el.scrollHeight;
    }
  }

  function bindChatAutoScroll() {
    var el = document.getElementById("chat_thread_container");
    if (!el || autoScrollBound) return;
    autoScrollBound = true;

    var observer = new MutationObserver(function () {
      requestAnimationFrame(function () {
        scrollChatToBottom(true);
      });
    });
    observer.observe(el, { childList: true, subtree: true });
    scrollChatToBottom(false);
  }

  function bindEnterToSend() {
    if (enterBound) return;
    enterBound = true;

    document.addEventListener(
      "keydown",
      function (e) {
        var target = e.target;
        if (!target || target.id !== "chat_input") return;
        if (e.isComposing) return;
        if (e.key !== "Enter" && e.keyCode !== 13) return;
        if (e.shiftKey) return;

        e.preventDefault();
        e.stopPropagation();

        var btn = document.getElementById("send");
        if (btn) btn.click();
      },
      true
    );
  }

  function initChatUi() {
    bindEnterToSend();
    bindChatAutoScroll();
  }

  function registerShinyHandlers() {
    if (typeof Shiny === "undefined") return;
    Shiny.addCustomMessageHandler("scrollChatToBottom", function () {
      requestAnimationFrame(function () {
        scrollChatToBottom(true);
      });
    });
  }

  registerShinyHandlers();
  bindEnterToSend();

  function onShinyConnected() {
    initChatUi();
  }

  if (window.jQuery) {
    jQuery(document).on("shiny:connected", onShinyConnected);
    if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
      onShinyConnected();
    }
  } else {
    document.addEventListener("DOMContentLoaded", initChatUi);
  }
})();

// --- www/custom.js ----------------------------------------------------

// give the browser one user-gesture so it will allow sound
let audioUnlocked = false;
document.addEventListener("click", () => { audioUnlocked = true; }, { once:true });

// helper – get elements only once
const bg    = document.getElementById("bgMusic");
const alarm = document.getElementById("shockAlarm");

document.addEventListener("click", function () {
  audioUnlocked = true;
  if (bg && bg.paused) {
    bg.muted = false;
    bg.currentTime = 0;
    bg.play().catch(()=>{});
  }
}, { once:true });


// 1) toggle background music -------------------------------------------------
Shiny.addCustomMessageHandler("toggle-music", function(msg) {
  if (!bg) return;
  if (bg.paused) {               // if paused → play
    bg.muted = false;
    bg.currentTime = 0;
    bg.play().catch(()=>{});     // swallow promise warnings
  } else {                       // if playing → pause
    bg.pause();
  }
});

// 2) play siren immediately when a NEW shock arrives -------------------------
Shiny.addCustomMessageHandler("play-shock", function(msg) {
  if (!alarm || !audioUnlocked) return;   // need one user click first
  alarm.muted       = false;              // un-mute
  alarm.currentTime = 0;                  // rewind
  alarm.play().catch(()=>{});
});

// 3) stop the siren when the shock fades or on Reset -------------------------
Shiny.addCustomMessageHandler("stop-shock", function(msg) {
  if (!alarm) return;
  alarm.pause();
  alarm.currentTime = 0;                  // rewind so next play starts at 0
});

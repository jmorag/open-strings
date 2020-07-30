function get_svg_fingerings(svg) {
  let svg_fingerings = [];
  svg
    .querySelectorAll("g.vf-stavenote>g.vf-modifiers")
    .forEach(m =>
      m.querySelectorAll("text").forEach(f => svg_fingerings.push(f))
    );
  svg_fingerings.forEach(
    f => f.textContent === "-1" && f.setAttribute("visibility", "hidden")
  );
  return svg_fingerings;
}

function get_svg_noteheads(svg) {
  let svg_noteheads = [];
  svg.querySelectorAll("g.vf-stavenote").forEach(
    n =>
      // filter rests
      n.querySelector("g.vf-modifiers").childElementCount > 0 &&
      n
        .querySelectorAll("g.vf-notehead>path")
        .forEach(n => svg_noteheads.push(n))
  );
  return svg_noteheads;
}

async function render_musicxml(xml, nodeId) {
  const osmd = new opensheetmusicdisplay.OpenSheetMusicDisplay(nodeId, {
    autoResize: true,
    backend: "svg",
    drawingParameters: "compact",
    drawPartNames: false,
    drawTitle: true,
    drawFingerings: true,
    fingeringPosition: "left",
    setWantedStemDirectionByXml: true,
    drawMeasureNumbers: false,
    autoBeam: false,
    pageFormat: "Endless"
  });

  xml.querySelectorAll("note").forEach(function(note) {
    if (note.querySelector("rest")) {
      return;
    }
    if (note.querySelector("notations") === null) {
      let notations = xml.createElement("notations");
      let technical = xml.createElement("technical");
      let fingering = xml.createElement("fingering");
      fingering.appendChild(xml.createTextNode("-1"));
      technical.appendChild(fingering);
      notations.appendChild(technical);
      note.appendChild(notations);
    } else if (note.querySelector("notations>technical") === null) {
      let technical = xml.createElement("technical");
      let fingering = xml.createElement("fingering");
      fingering.appendChild(xml.createTextNode("-1"));
      technical.appendChild(fingering);
      note.querySelector("notations").appendChild(technical);
    } else if (note.querySelector("notations>technical>fingering") === null) {
      let fingering = xml.createElement("fingering");
      fingering.appendChild(xml.createTextNode("-1"));
      note.querySelector("notations>technical").appendChild(fingering);
    }
  });
  await osmd.load(xml);
  osmd.render;
  const unfocused = "#000000";
  const selected = "#34d8eb";
  let target = document.getElementById(nodeId);

  const observer = new MutationObserver(function(_) {
    const svg = document.querySelector("svg");
    const svg_noteheads = get_svg_noteheads(svg);
    const svg_fingerings = get_svg_fingerings(svg);
    console.log(svg_noteheads.length, svg_fingerings.length);

    let index = 0;
    svg_noteheads[index].setAttribute("fill", selected);

    const next = function() {
      if (index >= svg_noteheads.length - 1) return;
      svg_noteheads[index].setAttribute("fill", unfocused);
      index++;
      svg_noteheads[index].setAttribute("fill", selected);
    };

    const prev = function() {
      if (index <= 0) return;
      svg_noteheads[index].setAttribute("fill", unfocused);
      index--;
      svg_noteheads[index].setAttribute("fill", selected);
    };

    const handleKeypress = function(e) {
      console.log(e);
      let finger = svg_fingerings[index];
      const setFinger = n => {
        finger.textContent = n;
        xml.querySelectorAll("fingering")[index].textContent = n;
        finger.removeAttribute("visibility");
        next();
      };
      switch (e.key) {
        case "ArrowRight":
          next();
          break;
        case "ArrowLeft":
          prev();
          break;
        case "`":
          setFinger("0");
          break;
        case "0":
          setFinger("0");
          break;
        case "1":
          setFinger("1");
          break;
        case "2":
          setFinger("2");
          break;
        case "3":
          setFinger("3");
          break;
        case "4":
          setFinger("4");
          break;
        case "Backspace":
          finger.textContent = "-1";
          finger.setAttribute("visibility", "hidden");
          prev();
          break;
        case "Enter":
          break;
      }
    };
    window.addEventListener("keydown", handleKeypress);
  });
  observer.observe(target, { childList: true });
}

function clean_xml(xml) {
  xml
    .querySelectorAll("fingering")
    .forEach(
      node => node.textContent === "-1" && node.parentNode.removeChild(node)
    );
}

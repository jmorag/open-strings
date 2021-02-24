class FingeringEditor {
  constructor(nodeId, clean) {
    this.clean = clean;
    this.dom_node = document.getElementById(nodeId);
    this.xml = null;
    this.index = 0;
    this.svg_stavenotes = null;
    this.svg_fingerings = null;
    this.svg_noteheads = null;
    this.svg_strings = null;
    this.unfocused = "#000000";
    this.focused = "#007bff";
    this.handleKeypress = this.handleKeypress.bind(this);
    this.handleClick = this.handleClick.bind(this);
    // Defined here so that we can disconnect it when calling clear
    this.observer = new MutationObserver((_) => {
      const svg = document.querySelector("svg");
      this.set_stavenotes(svg);
      this.set_noteheads(svg);
      this.set_fingerings_and_strings(svg);
      if (clean) this.resetFingerings();
    });
  }

  static arabicToRoman(n) {
    switch (n) {
      case "1":
        return "I";
      case "2":
        return "II";
      case "3":
        return "III";
      case "4":
        return "IV";
    }
    return "X";
  }

  get musicxml() {
    // TODO: consider not removing dummy xml nodes
    if (this.xml === null) {
      return null;
    }
    const xml = this.xml.cloneNode(true);

    // Remove empty fingerings
    xml
      .querySelectorAll("notations>technical>fingering")
      .forEach((fingering) => {
        if (fingering.textContent === "-1") {
          const technical = fingering.parentNode;
          const notations = technical.parentNode;
          const note = notations.parentNode;
          technical.removeChild(fingering);
          if (!technical.childElementCount) {
            notations.removeChild(technical);
            if (!notations.childElementCount) {
              note.removeChild(notations);
            }
          }
        }
      });

    // Remove empty strings
    xml.querySelectorAll("notations>technical>string").forEach((string) => {
      if (string.textContent === "X") {
        const technical = string.parentNode;
        const notations = technical.parentNode;
        const note = notations.parentNode;
        technical.removeChild(string);
        if (!technical.childElementCount) {
          notations.removeChild(technical);
          if (!notations.childElementCount) {
            note.removeChild(notations);
          }
        }
      }
    });
    return xml;
  }

  static parse_xml(xml_string, offset) {
    const parser = new DOMParser();
    const xml = parser.parseFromString(xml_string, "application/xml");
    // adjust measure numbers
    if (offset) {
      xml.querySelectorAll("measure").forEach((measure, i) => {
        if (measure.getAttribute("implicit") !== "yes") {
          measure.setAttribute("number", offset + i + 1);
        }
      });
    }
    const notes = xml.querySelectorAll("note");
    for (let i = notes.length - 1; i >= 0; i--) {
      const note = notes[i];
      if (note.querySelector("rest")) {
        continue;
      }
      // add -1 fingerings and strings to unfingered notes
      if (note.querySelector("notations") === null) {
        let notations = xml.createElement("notations");
        let technical = xml.createElement("technical");
        let fingering = xml.createElement("fingering");
        let string = xml.createElement("string");
        fingering.appendChild(xml.createTextNode("-1"));
        string.appendChild(xml.createTextNode("-1"));
        technical.appendChild(fingering);
        technical.appendChild(string);
        notations.appendChild(technical);
        note.appendChild(notations);
      } else if (note.querySelector("notations>technical") === null) {
        let technical = xml.createElement("technical");
        let fingering = xml.createElement("fingering");
        let string = xml.createElement("string");
        fingering.appendChild(xml.createTextNode("-1"));
        string.appendChild(xml.createTextNode("-1"));
        technical.appendChild(fingering);
        technical.appendChild(string);
        note.querySelector("notations").appendChild(technical);
      } else if (note.querySelector("notations>technical>fingering") === null) {
        let fingering = xml.createElement("fingering");
        fingering.appendChild(xml.createTextNode("-1"));
        note.querySelector("notations>technical").appendChild(fingering);
      } else if (note.querySelector("notations>technical>string") === null) {
        let string = xml.createElement("string");
        string.appendChild(xml.createTextNode("-1"));
        note.querySelector("notations>technical").appendChild(string);
      }
    }
    return xml;
  }

  set_fingerings_and_strings(svg) {
    const isFingering = (e) => {
      return (
        e.tagName === "text" &&
        ["-1", "0", "1", "2", "3", "4"].contains(e.textContent)
      );
    };

    let i = 0;
    this.svg_fingerings = [];
    this.svg_strings = [];
    for (const s of this.svg_stavenotes) {
      let modifiers = Array.from(s.querySelector("g.vf-modifiers").children);
      let j = i; // save index so we can reset for string numbers
      let fIndex = modifiers.findIndex(isFingering);
      if (fIndex === -1) {
        continue; // probably a rest
      }
      // svg finger numbers
      for (let f = modifiers[fIndex]; isFingering(f); f = modifiers[++fIndex]) {
        f.setAttribute("index", i);
        i++;
        // hide bogus fingerings
        f.textContent === "-1" && f.setAttribute("visibility", "hidden");
        this.svg_fingerings.push(f);
      }

      // svg string numbers
      i = j; // reset i to beginning of this chord
      for (let f = modifiers[fIndex]; fIndex < modifiers.length; f = modifiers[++fIndex]) {
        // skip all non-text elements
        if (f.tagName !== "path") {
          f.setAttribute("index", i);
          // hide bogus string numbers
          f.textContent === "-1" && f.setAttribute("visibility", "hidden");
          i++;
          this.svg_strings.push(f);
        }
      }
    }
  }

  set_noteheads(svg) {
    let i_xml = 0;
    let i_svg = 0;
    const xml_noteheads = this.xml.querySelectorAll("note");
    this.svg_noteheads = [];
    this.svg_stavenotes.forEach((n) =>
      n.children[0] // don't descend into nested grace notes
        .querySelectorAll("g.vf-notehead>path")
        .forEach((note) => {
          if (!xml_noteheads[i_xml].querySelector("rest")) {
            note.setAttribute("index", i_svg);
            i_svg++;
            this.svg_noteheads.push(note);
          }
          i_xml++;
        })
    );
  }

  set_stavenotes(svg) {
    this.svg_stavenotes = [];
    // In the svg, grace notes are children of the main note they are
    // attached to, even though they precede it musically
    svg.querySelectorAll("svg>g.vf-stavenote").forEach((n) => {
      n.querySelectorAll("g.vf-modifiers>g.vf-stavenote").forEach((grace) =>
        this.svg_stavenotes.push(grace)
      );
      this.svg_stavenotes.push(n);
    });
  }

  next() {
    if (this.index >= this.svg_noteheads.length - 1) return;
    this.svg_noteheads[this.index].setAttribute("fill", this.unfocused);
    this.index++;
    this.svg_noteheads[this.index].setAttribute("fill", this.focused);
  }

  prev() {
    if (this.index <= 0) return;
    this.svg_noteheads[this.index].setAttribute("fill", this.unfocused);
    this.index--;
    this.svg_noteheads[this.index].setAttribute("fill", this.focused);
  }

  setFinger(n) {
    console.log("Setting finger ", this.index, " to ", n);
    let finger = this.svg_fingerings[this.index];
    finger.textContent = n;
    this.xml.querySelectorAll("fingering")[this.index].textContent = n;
    if (n === "-1") {
      finger.setAttribute("visibility", "hidden");
    } else {
      finger.removeAttribute("visibility");
    }
  }

  setString(n) {
    let string = this.svg_strings[this.index];
    string.textContent = this.constructor.arabicToRoman(n);
    this.xml.querySelectorAll("string")[this.index].textContent = n;

    if (n === "-1") {
      string.setAttribute("visibility", "hidden");
    } else {
      string.removeAttribute("visibility");
    }
  }

  resetFingerings() {
    this.svg_fingerings.forEach((finger) => {
      finger.textContent = "-1";
      finger.setAttribute("visibility", "hidden");
    });
    this.svg_strings.forEach((string) => {
      string.textContent = "-1";
      string.setAttribute("visibility", "hidden");
    });
    this.xml.querySelectorAll("fingering").forEach((finger) => {
      finger.textContent = "-1";
    });
    this.xml.querySelectorAll("technical>string").forEach((string) => {
      string.textContent = "-1";
    });
    this.svg_noteheads[this.index].setAttribute("fill", this.unfocused);
    if (!this.clean) {
      this.svg_noteheads[0].setAttribute("fill", this.focused);
      this.index = 0;
    }
  }

  handleKeypress(e) {
    if (document.activeElement.tagName !== "svg") return;
    console.log(e);
    switch (e.code) {
      case "ArrowRight":
        this.next();
        break;
      case "ArrowLeft":
        this.prev();
        break;
      // lies conveniently to the left of 1 on most keyboards
      case "Backquote":
        this.setFinger("0");
        this.next();
        break;
      case "Digit0":
        this.setFinger("0");
        this.next();
        break;
      case "Digit1":
        if (e.shiftKey) this.setString("1");
        else this.setFinger("1");
        this.next();
        break;
      case "Digit2":
        if (e.shiftKey) this.setString("2");
        else this.setFinger("2");
        this.next();
        break;
      case "Digit3":
        if (e.shiftKey) this.setString("3");
        else this.setFinger("3");
        this.next();
        break;
      case "Digit4":
        if (e.shiftKey) this.setString("4");
        else this.setFinger("4");
        this.next();
        break;
      case "Backspace":
        this.setFinger("-1");
        this.setString("-1");
        break;
      case "Enter":
        break;
    }
  }

  handleClick({ target }) {
    if (document.activeElement.tagName !== "svg") return;
    let target_ix = parseInt(target?.getAttribute("index"));
    if (!isNaN(target_ix)) {
      this.svg_noteheads[this.index].setAttribute("fill", this.unfocused);
      this.svg_noteheads[target_ix].setAttribute("fill", this.focused);
      this.index = target_ix;
    }
  }

  clear() {
    this.observer.disconnect();
    this.dom_node.innerHTML = "";
  }

  hide() {
    this.dom_node.setAttribute("style", "visibility: hidden");
  }

  unHide() {
    this.dom_node.removeAttribute("style");
  }

  enable() {
    const svg = document.querySelector("svg");
    // Allow the svg container to be focused
    svg.setAttribute("tabindex", "0");
    svg.focus({ preventScroll: true });
    this.svg_noteheads[0].setAttribute("fill", this.focused);
    window.addEventListener("keydown", this.handleKeypress);
    window.addEventListener("mouseup", this.handleClick);
  }

  async render(xml_string, start_measure) {
    this.index = 0;
    const offset = parseInt(start_measure, 10) - 1;
    this.xml = this.constructor.parse_xml(xml_string, offset);
    window.removeEventListener("keydown", this.handleKeypress);
    window.removeEventListener("mouseup", this.handleClick);
    const osmd = new opensheetmusicdisplay.OpenSheetMusicDisplay(
      this.dom_node,
      {
        autoResize: true,
        backend: "svg",
        drawingParameters: "compacttight",
        drawPartNames: false,
        drawTitle: false,
        drawFingerings: true,
        fingeringPosition: "left",
        setWantedStemDirectionByXml: true,
        drawMeasureNumbers: !!start_measure,
        useXMLMeasureNumbers: !!start_measure,
        autoBeam: false,
        pageFormat: "Endless",
      }
    );
    await osmd.load(this.xml);
    osmd.render();
    this.observer.observe(this.dom_node, { childList: true });
  }
}

const arabicToRoman = n => {
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
  return "NOSTRING";
};

const romanToArabic = n => {
  switch (n) {
    case "I":
      return "1";
    case "II":
      return "2";
    case "III":
      return "3";
    case "IV":
      return "4";
  }
  return n;
};

class FingeringEditor {
  constructor(nodeId) {
    this.dom_node = document.getElementById(nodeId);
    this.xml = null;
    this.index = 0;
    this.svg_fingerings = null;
    this.svg_noteheads = null;
    this.svg_strings = null;
    this.unfocused = "#000000";
    this.focused = "#007bff";
    this.handleKeypress = this.handleKeypress.bind(this);
    this.handleClick = this.handleClick.bind(this);
    // Defined here so that we can disconnect it when calling clear
    this.observer = new MutationObserver(_ => {
      const svg = document.querySelector("svg");
      this.set_noteheads(svg);
      this.set_fingerings(svg);
      this.set_strings(svg);
    });
  }

  get musicxml() {
    if (this.xml === null) {
      return null;
    }
    const xml = this.xml.cloneNode(true);
    // Move strings from lyrics to string element inside notations>technical
    xml.querySelectorAll("lyric.string").forEach(string_lyric => {
      const n = romanToArabic(string_lyric.firstElementChild.textContent);
      if (n !== "NOSTRING") {
        let string_node = string_lyric.parentNode.querySelector(
          "notations>technical>string"
        );
        if (string_node) {
          string_node.textContent = n;
        } else {
          string_node = xml.createElement("string");
          string_node.textContent = n;
          string_lyric.parentNode
            .querySelector("notations>technical")
            .appendChild(string_node);
        }
        console.log("Set string in xml to ", n);
      }
      string_lyric.parentNode.removeChild(string_lyric);
    });

    // Remove empty fingerings
    xml.querySelectorAll("fingering").forEach(fingering => {
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
    return xml;
  }

  static parse_xml(xml_string) {
    const parser = new DOMParser();
    const xml = parser.parseFromString(xml_string, "application/xml");
    xml.querySelectorAll("note").forEach(note => {
      if (note.querySelector("rest")) {
        return;
      }
      // OSMD doesn't display string numbers so we display them as lyrics instead
      const lyric = xml.createElement("lyric");
      lyric.setAttribute("class", "string");
      const text = xml.createElement("text");
      lyric.appendChild(text);

      text.textContent = arabicToRoman(
        note.querySelector("notations>technical>string")?.textContent
      );
      note.appendChild(lyric);

      // add -1 fingerings to unfingered notes
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
    return xml;
  }

  set_fingerings(svg) {
    let i = 0;
    this.svg_fingerings = [];
    svg.querySelectorAll("g.vf-stavenote>g.vf-modifiers").forEach(m =>
      m.querySelectorAll("text").forEach(f => {
        f.setAttribute("index", i);
        i++;
        // hide bogus fingerings
        f.textContent === "-1" && f.setAttribute("visibility", "hidden");
        this.svg_fingerings.push(f);
      })
    );
  }

  set_strings(svg) {
    let i = 0;
    this.svg_strings = [];
    svg.querySelectorAll("svg>text").forEach(s => {
      if (["I", "II", "III", "IV", "NOSTRING"].contains(s.textContent)) {
        s.setAttribute("index", i);
        s.textContent === "NOSTRING" && s.setAttribute("visibility", "hidden");
        this.svg_strings.push(s);
      }
    });
  }

  set_noteheads(svg) {
    let i = 0;
    this.svg_noteheads = [];
    svg.querySelectorAll("g.vf-stavenote").forEach(
      n =>
        // filter rests
        n.querySelector("g.vf-modifiers").childElementCount > 0 &&
        n.querySelectorAll("g.vf-notehead>path").forEach(note => {
          note.setAttribute("index", i);
          i++;
          this.svg_noteheads.push(note);
        })
    );
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
    let finger = this.svg_fingerings[this.index];
    finger.textContent = n;
    this.xml.querySelectorAll("fingering")[this.index].textContent = n;
    if (n === "-1") {
      finger.setAttribute("visibility", "hidden");
    } else {
      finger.removeAttribute("visibility");
    }
    this.next();
  }

  setString(n) {
    let string = this.svg_strings[this.index];
    string.textContent = n;
    let string_num = romanToArabic(n);
    this.xml.querySelectorAll("lyric.string>text")[
      this.index
    ].textContent = string_num;

    if (n === "NOSTRING") {
      string.setAttribute("visibility", "hidden");
    } else {
      string.removeAttribute("visibility");
    }
    this.next();
  }

  resetFingerings() {
    this.svg_fingerings.forEach(finger => {
      finger.textContent = "-1";
      finger.removeAttribute("visibility");
    });
    this.xml.querySelectorAll("fingering").forEach(finger => {
      finger.textContent = "-1";
    });
  }

  handleKeypress(e) {
    if (document.activeElement.tagName !== "svg") return;
    switch (e.key) {
      case "ArrowRight":
        this.next();
        break;
      case "ArrowLeft":
        this.prev();
        break;
      // lies conveniently to the left of 1 on most keyboards
      case "`":
        this.setFinger("0");
        break;
      case "0":
        this.setFinger("0");
        break;
      case "1":
        this.setFinger("1");
        break;
      case "2":
        this.setFinger("2");
        break;
      case "3":
        this.setFinger("3");
        break;
      case "4":
        this.setFinger("4");
        break;
      case "Backspace":
        this.setFinger("-1");
        this.prev();
        this.setString("NOSTRING");
        this.prev();
        break;
      case "!":
        this.setString("I");
        break;
      case "@":
        this.setString("II");
        break;
      case "#":
        this.setString("III");
        break;
      case "$":
        this.setString("IV");
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

  enable() {
    const svg = document.querySelector("svg");
    // Allow the svg container to be focused
    svg.setAttribute("tabindex", "0");
    svg.focus({ preventScroll: true });
    this.svg_noteheads[0].setAttribute("fill", this.focused);
    window.addEventListener("keydown", this.handleKeypress);
    window.addEventListener("mouseup", this.handleClick);
  }

  async render(xml_string) {
    this.index = 0;
    this.xml = this.constructor.parse_xml(xml_string);
    window.removeEventListener("keydown", this.handleKeypress);
    window.removeEventListener("mouseup", this.handleClick);
    const osmd = new opensheetmusicdisplay.OpenSheetMusicDisplay(
      this.dom_node,
      {
        autoResize: true,
        backend: "svg",
        drawingParameters: "compact",
        drawPartNames: false,
        drawTitle: false,
        drawFingerings: true,
        fingeringPosition: "left",
        setWantedStemDirectionByXml: true,
        drawMeasureNumbers: false,
        autoBeam: false,
        pageFormat: "Endless"
      }
    );
    await osmd.load(this.xml);
    osmd.render();
    this.observer.observe(this.dom_node, { childList: true });
  }
}

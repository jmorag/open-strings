class FingeringEditor {
  constructor(nodeId) {
    this.nodeId = nodeId;
    this.xml = null;
    this.index = 0;
    this.svg_fingerings = null;
    this.svg_noteheads = null;
    this.unfocused = "#000000";
    this.focused = "#34d8eb";
    this.handleKeypress = this.handleKeypress.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  get musicxml() {
    const xml = this.xml.cloneNode(true);
    xml.querySelectorAll("fingering").forEach(function(fingering) {
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
    return xml;
  }

  set_fingerings(svg) {
    let i = 0;
    let svg_fingerings = [];
    svg.querySelectorAll("g.vf-stavenote>g.vf-modifiers").forEach(m =>
      m.querySelectorAll("text").forEach(f => {
        f.setAttribute("index", i);
        i++;
        // hide bogus fingerings
        f.textContent === "-1" && f.setAttribute("visibility", "hidden");
        svg_fingerings.push(f);
      })
    );
    this.svg_fingerings = svg_fingerings;
  }

  set_noteheads(svg) {
    let i = 0;
    let svg_noteheads = [];
    svg.querySelectorAll("g.vf-stavenote").forEach(
      n =>
        // filter rests
        n.querySelector("g.vf-modifiers").childElementCount > 0 &&
        n.querySelectorAll("g.vf-notehead>path").forEach(note => {
          note.setAttribute("index", i);
          i++;
          svg_noteheads.push(note);
        })
    );
    svg_noteheads[0].setAttribute("fill", this.focused);
    this.svg_noteheads = svg_noteheads;
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
    finger.removeAttribute("visibility");
    this.next();
  }

  handleKeypress(e) {
    let finger = this.svg_fingerings[this.index];
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
        finger.setAttribute("visibility", "hidden");
        this.prev();
        break;
      case "Enter":
        break;
    }
  }

  handleClick({ target }) {
    let target_ix = parseInt(target?.getAttribute("index"));
    if (!isNaN(target_ix)) {
      this.svg_noteheads[this.index].setAttribute("fill", this.unfocused);
      this.svg_noteheads[target_ix].setAttribute("fill", this.focused);
      this.index = target_ix;
    }
  }

  async render(xml_string) {
    this.index = 0;
    this.xml = this.constructor.parse_xml(xml_string);
    window.removeEventListener("keydown", this.handleKeypress);
    window.removeEventListener("mouseup", this.handleClick);
    const target = document.getElementById(this.nodeId);
    const osmd = new opensheetmusicdisplay.OpenSheetMusicDisplay(this.nodeId, {
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
    await osmd.load(this.xml);
    osmd.render();
    const observer = new MutationObserver(_ => {
      const svg = document.querySelector("svg");
      this.set_noteheads(svg);
      this.set_fingerings(svg);
    });
    observer.observe(target, { childList: true });
    window.addEventListener("keydown", this.handleKeypress);
    window.addEventListener("mouseup", this.handleClick);
  }
}

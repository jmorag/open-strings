SVGPathElement.prototype.select = function () {
  this.setAttribute("data-selected", "");
};

SVGPathElement.prototype.deselect = function () {
  this.removeAttribute("data-selected");
};

class FingeringEditor {
  constructor(nodeId, clean) {
    this.clean = clean;
    this.dom_node = document.getElementById(nodeId);
    this.xml = null;
    // emacs conventions for point and mark for note selection
    this.point = 0;
    this.mark = 0;
    this.svg_stavenotes = null;
    this.svg_fingerings = null;
    this.svg_noteheads = null;
    this.svg_strings = null;
    this.handleKeypress = this.handleKeypress.bind(this);
    // We need this extra variable so the state persists window size change
    this.enabled = false;
    // Defined here so that we can disconnect it when calling clear
    this.observer = new MutationObserver((mutation) => {
      const svg = document.querySelector("svg");
      this.set_svg_notes(svg);
      this.set_fingerings_and_strings(svg);
      if (this.clean) this.resetFingerings();
      if (this.enabled) this.enable();
      this.setupDrag(svg);
    });
    this.cancelDrag = null;
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

  *selected() {
    const left = Math.min(this.point, this.mark);
    const right = Math.max(this.point, this.mark);
    for (let i = left; i <= right; ++i) yield i;
    return;
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
      if (string.textContent === "-1") {
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

    // Indicate which notes were selected
    const non_rest_notes = xml.querySelectorAll("[svg-index]");
    for (const i of this.selected()) {
      non_rest_notes[i].setAttribute("data-selected", "");
    }
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
    // OSMD re-renders on window size change, destroying all
    // annotations. Fortunately, they are persisted on write to the
    // underlying musicxml, so we can recover them and re-apply them
    // here
    const xml_fingerings = this.xml.querySelectorAll("fingering");
    const xml_strings = this.xml.querySelectorAll("string");
    this.svg_fingerings = [];
    this.svg_strings = [];
    for (const s of this.svg_stavenotes) {
      let modifiers = Array.from(s.querySelector("g.vf-modifiers").children);
      let j = i; // save index so we can reset for string numbers
      let fIndex = modifiers.findIndex(isFingering);
      if (fIndex === -1) {
        continue; // Fingering not found, probably a rest
      }
      // svg finger numbers
      for (let f = modifiers[fIndex]; isFingering(f); f = modifiers[++fIndex]) {
        f.setAttribute("index", i);
        f.textContent = xml_fingerings[i].textContent;
        i++;
        // hide bogus fingerings
        f.textContent === "-1" && f.setAttribute("visibility", "hidden");
        this.svg_fingerings.push(f);
      }

      // svg string numbers
      i = j; // reset i to beginning of this chord
      for (
        let f = modifiers[fIndex];
        fIndex < modifiers.length;
        f = modifiers[++fIndex]
      ) {
        // skip all non-text elements
        if (f.tagName !== "path") {
          f.setAttribute("index", i);
          f.textContent = this.constructor.arabicToRoman(
            xml_strings[i].textContent
          );
          // hide bogus string numbers
          f.textContent === "X" && f.setAttribute("visibility", "hidden");
          i++;
          this.svg_strings.push(f);
        }
      }
    }
  }

  set_svg_notes(svg) {
    this.svg_stavenotes = [];
    // In the svg, grace notes are children of the main note they are
    // attached to, even though they precede it musically
    svg.querySelectorAll("svg>g.vf-stavenote").forEach((n) => {
      n.querySelectorAll("g.vf-modifiers>g.vf-stavenote").forEach((grace) =>
        this.svg_stavenotes.push(grace)
      );
      this.svg_stavenotes.push(n);
    });

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
            xml_noteheads[i_xml].setAttribute("svg-index", i_svg);
          }
          i_xml++;
        })
    );
  }

  next(shift = false) {
    if (this.point >= this.svg_noteheads.length - 1) {
      if (!shift) {
        while (this.mark < this.point) {
          this.svg_noteheads[this.mark++].deselect();
        }
      }
      return;
    }
    if (shift) {
      if (this.point < this.mark) {
        this.svg_noteheads[this.point].deselect();
      }
      this.point++;
      this.svg_noteheads[this.point].select();
      return;
    }
    for (const i of this.selected()) this.svg_noteheads[i].deselect();
    this.point++;
    this.mark = this.point;
    this.svg_noteheads[this.point].select();
  }

  prev(shift = false) {
    if (this.point <= 0) {
      if (!shift) {
        while (this.mark > this.point) {
          this.svg_noteheads[this.mark--].deselect();
        }
      }
      return;
    }
    if (shift) {
      if (this.point > this.mark) {
        this.svg_noteheads[this.point].deselect();
      }
      this.point--;
      this.svg_noteheads[this.point].select();
      return;
    }
    for (const i of this.selected()) this.svg_noteheads[i].deselect();
    this.point--;
    this.mark = this.point;
    this.svg_noteheads[this.point].select();
  }

  setFinger(n) {
    for (const index of this.selected()) {
      let finger = this.svg_fingerings[index];
      finger.textContent = n;
      this.xml.querySelectorAll("fingering")[index].textContent = n;
      if (n === "-1") {
        finger.setAttribute("visibility", "hidden");
      } else {
        finger.removeAttribute("visibility");
      }
    }
  }

  setString(n) {
    for (const index of this.selected()) {
      let string = this.svg_strings[index];
      string.textContent = this.constructor.arabicToRoman(n);
      this.xml.querySelectorAll("string")[index].textContent = n;
      if (n === "-1") {
        string.setAttribute("visibility", "hidden");
      } else {
        string.removeAttribute("visibility");
      }
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
    for (const i of this.selected()) this.svg_noteheads[i].deselect();
    if (!this.clean) {
      this.svg_noteheads[0].select();
      this.mark = 0;
      this.point = 0;
    }
  }

  handleKeypress(e) {
    if (document.activeElement.tagName !== "svg") return;
    // console.debug(e);
    switch (e.code) {
      case "ArrowRight":
        this.next(e.shiftKey);
        // console.debug([...this.selected()]);
        break;
      case "ArrowLeft":
        this.prev(e.shiftKey);
        // console.debug([...this.selected()]);
        break;
      // lies conveniently to the left of 1 on most keyboards
      case "Backquote":
        this.setFinger("0");
        this.point === this.mark && this.next();
        break;
      case "Digit0":
        this.setFinger("0");
        this.point === this.mark && this.next();
        break;
      case "Digit1":
        if (e.shiftKey) this.setString("1");
        else this.setFinger("1");
        this.point === this.mark && this.next();
        break;
      case "Digit2":
        if (e.shiftKey) this.setString("2");
        else this.setFinger("2");
        this.point === this.mark && this.next();
        break;
      case "Digit3":
        if (e.shiftKey) this.setString("3");
        else this.setFinger("3");
        this.point === this.mark && this.next();
        break;
      case "Digit4":
        if (e.shiftKey) this.setString("4");
        else this.setFinger("4");
        this.point === this.mark && this.next();
        break;
      case "Backspace":
        this.setFinger("-1");
        this.setString("-1");
        break;
      case "Enter":
        break;
    }
  }

  clear() {
    this.observer.disconnect();
    this.dom_node.innerHTML = "";
    this.cancelDrag();
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
    this.svg_noteheads[this.mark].select();
    window.addEventListener("keydown", this.handleKeypress);
    this.enabled = true;
  }

  disable() {
    const svg = document.querySelector("svg");
    svg.setAttribute("tabindex", "-1");
    this.svg_noteheads[this.mark].deselect();
    this.enabled = false;
    this.cancelDrag();
  }

  // see https://github.com/luncheon/svg-drag-select#usage-and-options
  setupDrag(svg) {
    const noteHeadSelector = ({ getIntersections }) =>
      getIntersections().filter(
        (element) =>
          element instanceof SVGPathElement &&
          element.getAttribute("index") !== null
      );

    const onSelectionStart = ({ svg, pointerEvent, cancel }) => {
      // for example: handles mouse left button only.
      if (pointerEvent.button !== 0) {
        cancel();
        return;
      }
      const selectedElements = svg.querySelectorAll("[data-selected]");
      selectedElements.forEach((element) => element.deselect());
    };

    const onSelectionChange = ({
      newlySelectedElements,
      newlyDeselectedElements,
    }) => {
      newlyDeselectedElements.forEach((element) => element.deselect());
      newlySelectedElements.forEach((element) => element.select());
    };

    const onSelectionEnd = ({ selectedElements }) => {
      const getIndex = (i) => {
        return +selectedElements[i].getAttribute("index");
      };
      if (selectedElements.length === 0) {
        this.point = 0;
        this.mark = 0;
      } else if (selectedElements.length === 1) {
        this.point = this.mark = getIndex(0);
      } else {
        this.point = getIndex(0);
        let i;
        for (i = 1; i < selectedElements.length; ++i) {
          if (getIndex(i) === getIndex(i - 1) + 1) this.mark = getIndex(i);
          else break;
        }
        // deselect all notes outside of contiguous region
        for (; i < selectedElements.length; ++i) {
          selectedElements[i].deselect();
        }
      }
    };

    const { cancel } = svgDragSelect({
      svg,
      selector: noteHeadSelector,
      onSelectionStart,
      onSelectionChange,
      onSelectionEnd,
    });

    this.cancelDrag = cancel;
  }

  async render(xml_string, start_measure) {
    const offset = +start_measure - 1;
    this.xml = this.constructor.parse_xml(xml_string, offset);
    window.removeEventListener("keydown", this.handleKeypress);
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

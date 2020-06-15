import {
  html,
  render,
  Component
} from "https://unpkg.com/htm/preact/standalone.module.js";
import Autosuggest from "https://unpkg.com/react-autosuggest/dist/standalone/autosuggest.js";

class UploadForm extends Component {
  state = { composer: "" };

  onInput(event) {
    console.log(event.target.value);
  }

  render() {
    return html`
      <form>
        <input type="text" value=${this.state.value} onInput=${this.onInput} />
      </form>
    `;
  }
}

render(
  html`
    <${UploadForm} />
  `,
  document.getElementById("upload-music")
);

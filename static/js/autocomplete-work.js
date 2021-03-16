Vue.component("autocomplete-work", {
  props: ["value", "csrf", "id", "label"],
  components: { Autocomplete },
  data: function () {
    return {
      work_id: 0,
      url: "",
      title: "",
      composer: "",
      composer_url: null,
      instrumentation: [],
      work_movements: [],
      imslp: true,
      show_alert: false,
      loading: false,
      validated: false,
      show_add_work_modal: false,
    };
  },
  computed: {
    existing_work_link: function () {
      return `/work/${this.work_id}`;
    },
  },
  template: `<div>
  <b-form-group class="required" description="Search by composer name and title, ex. beethoven violin concerto" v-bind:label-for="id" v-bind:label="label">
    <autocomplete
     id="work-title"
     @submit="autocompleteSubmit"
     default-value=""
     placeholder="Work"
     ref="autocomplete"
     v-bind:auto-select="true"
     v-bind:debounce-time="300"
     v-bind:get-result-value="(result) => result.label"
     v-bind:search="search"
     >
    </autocomplete>
  </b-form-group>
  <b-modal v-model="show_add_work_modal" title="Add work" size="xl">
    <b-alert v-bind:show="show_alert" variant="danger" dismissible @dismissed="reset">
      This work is already in the database. View it <a v-bind:href="existing_work_link">here</a>.
    </b-alert>
    <b-form novalidate>
      <b-form-group>
        <b-form-group label-for="piece-url" label="URL">
          <b-form-input
            type="url"
            id="piece-url"
            v-model.trim="url"
            placeholder="URL"
            autofocus
            autocomplete="on"
            v-bind:formatter="decodeURI"
            >
            <template v-slot:description>If the piece is on <a href="https://imslp.org/wiki/Main_Page">IMSLP</a>, enter its url and we'll autofill the rest. Otherwise, leave this blank.
            </template>
          </b-form-input>
        </b-form-group>
        <b-form-group v-bind:validated="validated">
          <b-form-row>
            <b-form-group label-for="piece-title" label="Title" class="col">
              <b-form-input
                type="text"
                autocomplete="off"
                id="piece-title"
                v-model.trim="title"
                placeholder="Title"
                required
                >
              </b-form-input>
            </b-form-group>
            <b-form-group label-for="piece-composer" label="Composer" class="col">
              <autocomplete
                required
                ref="composer_autocomplete"
                id="piece-composer"
                @submit="composerAutocompleteSubmit"
                placeholder="Composer"
                auto-select
                v-bind:debounce-time="300"
                v-bind:get-result-value="result => result.label"
                v-bind:search="composerSearch"
                >
              </autocomplete>
              <template v-slot:description>Enter the composer as <em>last-name</em>, <em>first-name</em> <em>middle-name</em>.
              </template>
            </b-form-group>
          </b-form-row>
          <b-form-row>
            <b-form-group label-for="piece-movements" label="Movements" class="col">
              <b-input-group v-for="(movement, i) in work_movements" v-bind:key="i" v-bind:prepend="(i + 1).toString()" id="piece-movements">
                <b-form-input v-model.trim="work_movements[i]" v-bind:spellcheck="false" autocomplete="off">
                </b-form-input>
                <b-input-group-append>
                  <b-button tabindex="-1" variant="outline-secondary" @click="work_movements.splice(i, 1)">x
                  </b-button>
                </b-input-group-append>
              </b-input-group>
              <b-button @click="work_movements.splice(work_movements.length, 0, '')"
                block size="md" variant="outline-secondary">Add movement
              </b-button>
            </b-form-group>
            <b-form-group class="col">
              <label id="instrumentation-label" for="piece-instrumentation">Instrumentation <span style='color: #007bff'>(?)</span>
              </label>
              <b-form-tags
                v-bind:state="validated ? instrumentation.length > 0 : null"
                ref="instrumentationInput"
                v-model="instrumentation"
                input-id="piece-instrumentation"
                placeholder=""
                remove-on-delete
                separator=",;"
                v-bind:tag-validator="validatePart"
                invalid-tag-text="Unsupported part"
                duplicate-tag-text="Part already entered"
                >
              </b-form-tags>
              <b-form-invalid-feedback>Piece must have at least one part</b-form-invalid-feedback>
              <b-popover id="instrumentation-info" triggers="hover" noninteractive target="instrumentation-label">
                  <template v-slot:title>Enter string parts separated by commas, ex. <kbd>violin, violin 1, tutti violin 2, viola, cello, bass</kbd>. Instruments not preceded by a <kbd>solo</kbd> or <kbd>solo</kbd>default to solo parts. For convenience, you can use <kbd>strings</kbd> to refer to a standard orchestra string section and <kbd>quartet</kbd> for a string quartet.
                  </template>
              </b-popover>
            </b-form-group>
            </b-form-row>
          </b-form-group>
        </b-form-group>
      </b-form>
    <b-overlay v-bind:show="loading" rounded="sm" no-wrap fixed></b-overlay>
        <template v-slot:modal-footer>
        <b-form-group>
          <b-button variant="primary" @click="add_work">Add work to database</b-button>
          <b-button variant="secondary" @click="reset">Clear</b-button>
        </b-form-group>
        </template>
  </b-modal>
</div>`,

  methods: {
    async search(input) {
      if (input.length < 2) {
        return [];
      }
      const data = await fetch(`/api/works?term=${input}`);
      const suggestions = await data.json();
      return suggestions.length === 0 && this.work_id === 0
        ? [{ label: `"${input}" not found. Add piece data manually`, value: -1 }]
        : suggestions;
    },

    async autocompleteSubmit(input) {
      if (input === undefined) {
        this.$emit("autocomplete-submit", this.work_id);
        return;
      }
      if (input.value === -1) {
        this.show_add_work_modal = true;
        this.$refs.autocomplete.value = "";
        return;
      }
      this.$emit("autocomplete-submit", input.value);
    },

    composerAutocompleteSubmit({ label }) {
      this.composer = label;
    },

    async composerSearch(input) {
      if (input.length < 2) {
        return [];
      }
      const data = await fetch(`/api/composers?term=${input}`);
      const suggestions = await data.json();
      if (suggestions.length === 0) {
        this.composer = input;
      }
      return suggestions;
    },

    async add_work(e) {
      if (!(this.instrumentation.length > 0) || !this.composer || !this.title) {
        this.validated = true;
        return;
      }
      const body = {
        work_url: this.url.trim() || null,
        work_title: this.title.trim(),
        work_composer: this.composer.trim(),
        work_instrumentation: this.instrumentation,
        work_movements: this.work_movements.map((m) => m.trim()),
        composer_url: this.composer_url,
      };
      this.loading = true;
      const resp = await fetch("/api/add-work", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "X-XSRF-TOKEN": this.csrf,
        },
        body: JSON.stringify(body),
      });
      const json = await resp.json();
      this.loading = false;
      this.work_id = json.work_id;
      if (json.already_uploaded) {
        this.show_alert = true;
        return;
      }
      this.show_add_work_modal = false;
      this.$refs.autocomplete.value = json.label;
      this.$emit("autocomplete-submit", this.work_id);
    },

    validatePart(part) {
      const regex = /^(?:(?:(solo|tutti)\s*)?(?:(violin|viola|cello|bass))(\s+[1-9]\d*)?|strings|quartet)$/;
      return part.match(regex);
    },

    clear(e) {
      this.title = "";
      this.$refs.composer_autocomplete.value = this.composer = "";
      this.instrumentation = [];
      this.work_movements = [];
      this.imslp = true;
      this.show_alert = false;
      this.validated = false;
    },

    reset(e) {
      this.clear();
      this.url = "";
    },
  },

  watch: {
    url: async function (url) {
      this.clear();
      if (!url.includes("imslp.org")) {
        this.imslp = false;
        return;
      }
      const data = await fetch(`/api/imslp/${encodeURIComponent(url)}`);
      const json = await data.json();
      this.title = json.title;
      this.$refs.composer_autocomplete.value = this.composer = json.composer;
      this.composer_url = json.composer_url;
      this.instrumentation = json.instrumentation.filter(this.validatePart);
      this.work_movements = json.movements;
    },
  },
});

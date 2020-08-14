var Autocomplete = (function() {
  "use strict";
  function e(e, t) {
    if (!(e instanceof t))
      throw new TypeError("Cannot call a class as a function");
  }
  function t(e, t, n) {
    return (
      t in e
        ? Object.defineProperty(e, t, {
            value: n,
            enumerable: !0,
            configurable: !0,
            writable: !0
          })
        : (e[t] = n),
      e
    );
  }
  function n(e, t) {
    var n = Object.keys(e);
    if (Object.getOwnPropertySymbols) {
      var s = Object.getOwnPropertySymbols(e);
      t &&
        (s = s.filter(function(t) {
          return Object.getOwnPropertyDescriptor(e, t).enumerable;
        })),
        n.push.apply(n, s);
    }
    return n;
  }
  function s(e) {
    for (var s = 1; s < arguments.length; s++) {
      var o = null != arguments[s] ? arguments[s] : {};
      s % 2
        ? n(Object(o), !0).forEach(function(n) {
            t(e, n, o[n]);
          })
        : Object.getOwnPropertyDescriptors
        ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(o))
        : n(Object(o)).forEach(function(t) {
            Object.defineProperty(e, t, Object.getOwnPropertyDescriptor(o, t));
          });
    }
    return e;
  }
  var o = function(e, t) {
      return e.matches
        ? e.matches(t)
        : e.msMatchesSelector
        ? e.msMatchesSelector(t)
        : e.webkitMatchesSelector
        ? e.webkitMatchesSelector(t)
        : null;
    },
    i = function(e, t) {
      return e.closest
        ? e.closest(t)
        : (function(e, t) {
            for (var n = e; n && 1 === n.nodeType; ) {
              if (o(n, t)) return n;
              n = n.parentNode;
            }
            return null;
          })(e, t);
    },
    r = function(e) {
      return Boolean(e && "function" == typeof e.then);
    },
    u = function n() {
      var s = this,
        o = arguments.length > 0 && void 0 !== arguments[0] ? arguments[0] : {},
        u = o.search,
        l = o.autoSelect,
        a = void 0 !== l && l,
        c = o.setValue,
        d = void 0 === c ? function() {} : c,
        h = o.setAttribute,
        p = void 0 === h ? function() {} : h,
        f = o.onUpdate,
        b = void 0 === f ? function() {} : f,
        v = o.onSubmit,
        m = void 0 === v ? function() {} : v,
        g = o.onShow,
        w = void 0 === g ? function() {} : g,
        y = o.onHide,
        x = void 0 === y ? function() {} : y,
        L = o.onLoading,
        R = void 0 === L ? function() {} : L,
        S = o.onLoaded,
        I = void 0 === S ? function() {} : S;
      e(this, n),
        t(this, "value", ""),
        t(this, "searchCounter", 0),
        t(this, "results", []),
        t(this, "selectedIndex", -1),
        t(this, "handleInput", function(e) {
          var t = e.target.value;
          s.updateResults(t), (s.value = t);
        }),
        t(this, "handleKeyDown", function(e) {
          var t = e.key;
          switch (t) {
            case "Up":
            case "Down":
            case "ArrowUp":
            case "ArrowDown":
              var n =
                "ArrowUp" === t || "Up" === t
                  ? s.selectedIndex - 1
                  : s.selectedIndex + 1;
              e.preventDefault(), s.handleArrows(n);
              break;
            case "Tab":
              s.selectResult();
              break;
            case "Enter":
              var o = s.results[s.selectedIndex];
              s.selectResult(), s.onSubmit(o);
              break;
            case "Esc":
            case "Escape":
              s.hideResults(), s.setValue();
              break;
            default:
              return;
          }
        }),
        t(this, "handleFocus", function(e) {
          var t = e.target.value;
          s.updateResults(t), (s.value = t);
        }),
        t(this, "handleBlur", function() {
          s.hideResults();
        }),
        t(this, "handleResultMouseDown", function(e) {
          e.preventDefault();
        }),
        t(this, "handleResultClick", function(e) {
          var t = e.target,
            n = i(t, "[data-result-index]");
          if (n) {
            s.selectedIndex = parseInt(n.dataset.resultIndex, 10);
            var o = s.results[s.selectedIndex];
            s.selectResult(), s.onSubmit(o);
          }
        }),
        t(this, "handleArrows", function(e) {
          var t = s.results.length;
          (s.selectedIndex = ((e % t) + t) % t),
            s.onUpdate(s.results, s.selectedIndex);
        }),
        t(this, "selectResult", function() {
          var e = s.results[s.selectedIndex];
          e && s.setValue(e), s.hideResults();
        }),
        t(this, "updateResults", function(e) {
          var t = ++s.searchCounter;
          s.onLoading(),
            s.search(e).then(function(e) {
              t === s.searchCounter &&
                ((s.results = e),
                s.onLoaded(),
                0 !== s.results.length
                  ? ((s.selectedIndex = s.autoSelect ? 0 : -1),
                    s.onUpdate(s.results, s.selectedIndex),
                    s.showResults())
                  : s.hideResults());
            });
        }),
        t(this, "showResults", function() {
          s.setAttribute("aria-expanded", !0), s.onShow();
        }),
        t(this, "hideResults", function() {
          (s.selectedIndex = -1),
            (s.results = []),
            s.setAttribute("aria-expanded", !1),
            s.setAttribute("aria-activedescendant", ""),
            s.onUpdate(s.results, s.selectedIndex),
            s.onHide();
        }),
        t(this, "checkSelectedResultVisible", function(e) {
          var t = e.querySelector(
            '[data-result-index="'.concat(s.selectedIndex, '"]')
          );
          if (t) {
            var n = e.getBoundingClientRect(),
              o = t.getBoundingClientRect();
            o.top < n.top
              ? (e.scrollTop -= n.top - o.top)
              : o.bottom > n.bottom && (e.scrollTop += o.bottom - n.bottom);
          }
        }),
        (this.search = r(u)
          ? u
          : function(e) {
              return Promise.resolve(u(e));
            }),
        (this.autoSelect = a),
        (this.setValue = d),
        (this.setAttribute = p),
        (this.onUpdate = b),
        (this.onSubmit = m),
        (this.onShow = w),
        (this.onHide = x),
        (this.onLoading = R),
        (this.onLoaded = I);
    },
    l = 0,
    a = function() {
      var e =
        arguments.length > 0 && void 0 !== arguments[0] ? arguments[0] : "";
      return "".concat(e).concat(++l);
    };
  const c = (function(e, t, n, s, o, i, r, u, l, a) {
    "boolean" != typeof r && ((l = u), (u = r), (r = !1));
    const c = "function" == typeof n ? n.options : n;
    let d;
    if (
      (e &&
        e.render &&
        ((c.render = e.render),
        (c.staticRenderFns = e.staticRenderFns),
        (c._compiled = !0),
        o && (c.functional = !0)),
      s && (c._scopeId = s),
      i
        ? ((d = function(e) {
            (e =
              e ||
              (this.$vnode && this.$vnode.ssrContext) ||
              (this.parent &&
                this.parent.$vnode &&
                this.parent.$vnode.ssrContext)) ||
              "undefined" == typeof __VUE_SSR_CONTEXT__ ||
              (e = __VUE_SSR_CONTEXT__),
              t && t.call(this, l(e)),
              e && e._registeredComponents && e._registeredComponents.add(i);
          }),
          (c._ssrRegister = d))
        : t &&
          (d = r
            ? function(e) {
                t.call(this, a(e, this.$root.$options.shadowRoot));
              }
            : function(e) {
                t.call(this, u(e));
              }),
      d)
    )
      if (c.functional) {
        const e = c.render;
        c.render = function(t, n) {
          return d.call(n), e(t, n);
        };
      } else {
        const e = c.beforeCreate;
        c.beforeCreate = e ? [].concat(e, d) : [d];
      }
    return n;
  })(
    {
      render: function() {
        var e = this,
          t = e.$createElement,
          n = e._self._c || t;
        return n(
          "div",
          { ref: "root" },
          [
            e._t(
              "default",
              [
                n("div", e._b({}, "div", e.rootProps, !1), [
                  n(
                    "input",
                    e._g(
                      e._b(
                        {
                          ref: "input",
                          on: {
                            input: e.handleInput,
                            keydown: e.core.handleKeyDown,
                            focus: e.core.handleFocus,
                            blur: e.core.handleBlur
                          }
                        },
                        "input",
                        e.inputProps,
                        !1
                      ),
                      e.$listeners
                    )
                  ),
                  e._v(" "),
                  n(
                    "ul",
                    e._g(
                      e._b({ ref: "resultList" }, "ul", e.resultListProps, !1),
                      e.resultListListeners
                    ),
                    [
                      e._l(e.results, function(t, s) {
                        return [
                          e._t(
                            "result",
                            [
                              n(
                                "li",
                                e._b(
                                  { key: e.resultProps[s].id },
                                  "li",
                                  e.resultProps[s],
                                  !1
                                ),
                                [
                                  e._v(
                                    "\n              " +
                                      e._s(e.getResultValue(t)) +
                                      "\n            "
                                  )
                                ]
                              )
                            ],
                            { result: t, props: e.resultProps[s] }
                          )
                        ];
                      })
                    ],
                    2
                  )
                ])
              ],
              {
                rootProps: e.rootProps,
                inputProps: e.inputProps,
                inputListeners: e.inputListeners,
                resultListProps: e.resultListProps,
                resultListListeners: e.resultListListeners,
                results: e.results,
                resultProps: e.resultProps
              }
            )
          ],
          2
        );
      },
      staticRenderFns: []
    },
    void 0,
    {
      name: "Autocomplete",
      inheritAttrs: !1,
      props: {
        search: { type: Function, required: !0 },
        baseClass: { type: String, default: "autocomplete" },
        autoSelect: { type: Boolean, default: !1 },
        getResultValue: {
          type: Function,
          default: function(e) {
            return e;
          }
        },
        defaultValue: { type: String, default: "" },
        debounceTime: { type: Number, default: 0 }
      },
      data: function() {
        var e,
          t,
          n,
          s,
          o = new u({
            search: this.search,
            autoSelect: this.autoSelect,
            setValue: this.setValue,
            onUpdate: this.handleUpdate,
            onSubmit: this.handleSubmit,
            onShow: this.handleShow,
            onHide: this.handleHide,
            onLoading: this.handleLoading,
            onLoaded: this.handleLoaded
          });
        return (
          this.debounceTime > 0 &&
            (o.handleInput =
              ((e = o.handleInput),
              (t = this.debounceTime),
              function() {
                var o = this,
                  i = arguments,
                  r = function() {
                    (s = null), n || e.apply(o, i);
                  },
                  u = n && !s;
                clearTimeout(s), (s = setTimeout(r, t)), u && e.apply(o, i);
              })),
          {
            core: o,
            value: this.defaultValue,
            resultListId: a("".concat(this.baseClass, "-result-list-")),
            results: [],
            selectedIndex: -1,
            expanded: !1,
            loading: !1,
            position: "below",
            resetPosition: !0
          }
        );
      },
      computed: {
        rootProps: function() {
          return {
            class: this.baseClass,
            style: { position: "relative" },
            "data-expanded": this.expanded,
            "data-loading": this.loading,
            "data-position": this.position
          };
        },
        inputProps: function() {
          return s(
            {
              class: "".concat(this.baseClass, "-input", " form-control"),
              value: this.value,
              role: "combobox",
              autocomplete: "off",
              autocapitalize: "off",
              autocorrect: "off",
              spellcheck: "false",
              "aria-autocomplete": "list",
              "aria-haspopup": "listbox",
              "aria-owns": this.resultListId,
              "aria-expanded": this.expanded ? "true" : "false",
              "aria-activedescendant":
                this.selectedIndex > -1
                  ? this.resultProps[this.selectedIndex].id
                  : ""
            },
            this.$attrs
          );
        },
        inputListeners: function() {
          return {
            input: this.handleInput,
            keydown: this.core.handleKeyDown,
            focus: this.core.handleFocus,
            blur: this.core.handleBlur
          };
        },
        resultListProps: function() {
          var e = "below" === this.position ? "top" : "bottom";
          return {
            id: this.resultListId,
            class: "".concat(this.baseClass, "-result-list"),
            role: "listbox",
            style: t(
              {
                position: "absolute",
                zIndex: 1,
                width: "100%",
                visibility: this.expanded ? "visible" : "hidden",
                pointerEvents: this.expanded ? "auto" : "none"
              },
              e,
              "100%"
            )
          };
        },
        resultListListeners: function() {
          return {
            mousedown: this.core.handleResultMouseDown,
            click: this.core.handleResultClick
          };
        },
        resultProps: function() {
          var e = this;
          return this.results.map(function(t, n) {
            return s(
              {
                id: "".concat(e.baseClass, "-result-").concat(n),
                class: "".concat(e.baseClass, "-result"),
                "data-result-index": n,
                role: "option"
              },
              e.selectedIndex === n ? { "aria-selected": "true" } : {}
            );
          });
        }
      },
      mounted: function() {
        document.body.addEventListener("click", this.handleDocumentClick);
      },
      beforeDestroy: function() {
        document.body.removeEventListener("click", this.handleDocumentClick);
      },
      updated: function() {
        var e, t, n, s;
        this.$refs.input &&
          this.$refs.resultList &&
          (this.resetPosition &&
            this.results.length > 0 &&
            ((this.resetPosition = !1),
            (this.position =
              ((e = this.$refs.input),
              (t = this.$refs.resultList),
              (n = e.getBoundingClientRect()),
              (s = t.getBoundingClientRect()),
              n.bottom + s.height > window.innerHeight &&
              window.innerHeight - n.bottom < n.top &&
              window.pageYOffset + n.top - s.height > 0
                ? "above"
                : "below"))),
          this.core.checkSelectedResultVisible(this.$refs.resultList));
      },
      methods: {
        setValue: function(e) {
          this.value = e ? this.getResultValue(e) : "";
        },
        handleUpdate: function(e, t) {
          (this.results = e),
            (this.selectedIndex = t),
            this.$emit("update", e, t);
        },
        handleShow: function() {
          this.expanded = !0;
        },
        handleHide: function() {
          (this.expanded = !1), (this.resetPosition = !0);
        },
        handleLoading: function() {
          this.loading = !0;
        },
        handleLoaded: function() {
          this.loading = !1;
        },
        handleInput: function(e) {
          (this.value = e.target.value), this.core.handleInput(e);
        },
        handleSubmit: function(e) {
          this.$emit("submit", e);
        },
        handleDocumentClick: function(e) {
          this.$refs.root.contains(e.target) || this.core.hideResults();
        }
      }
    },
    void 0,
    !1,
    void 0,
    !1,
    void 0,
    void 0,
    void 0
  );
  function d(e) {
    d.installed || ((d.installed = !0), e.component("Autocomplete", c));
  }
  var h,
    p = { install: d };
  return (
    "undefined" != typeof window
      ? (h = window.Vue)
      : "undefined" != typeof global && (h = global.Vue),
    h && h.use(p),
    (c.install = d),
    c
  );
})();

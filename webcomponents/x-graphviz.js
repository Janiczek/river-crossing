xtag.create('x-graphviz', class extends XTagElement {

  constructor () {
    super();
    this.graphviz = window['@hpcc-js/wasm'].graphviz;
  }

  set 'dot::attr' (dotString) {
    if (!dotString || dotString.length === 0) return;
    dotString = dotString.replace(/&quot;/g, '"');
    this.graphviz.layout(dotString, 'svg', 'dot')
      .then(svgString => {
        this.innerHTML = '';
        this.innerHTML = svgString;
        return svgString;
      })
      .then(svgString => {
        setTimeout(() => { // this takes care of waiting for the HTML parsing etc.
          const nodes = 
            [...document.querySelectorAll('.node g')]
              .filter(el => el.hasAttribute('id'));
          this.addHandlers(nodes);
          this.removeLinkAttributes(nodes);
        }, 0);
      });
  }

  addHandlers (nodes) {
    nodes.forEach(el => {
      el.addEventListener('click', this.handleClick);
    })
  }

  removeLinkAttributes(nodes) {
    nodes.forEach(el => {
      el.querySelectorAll('a')
        .forEach(linkEl => {
          linkEl.removeAttribute('xlink:href');
          linkEl.removeAttribute('xlink:title');
        });
    });
  }

  handleClick() {
    xtag.fireEvent(this, 'x-graphviz-node-click', {
      detail: {
        id: this.getAttribute('id').replace(/^a_/, '')
      }
    });
  }

});

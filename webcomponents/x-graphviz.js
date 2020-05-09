xtag.create('x-graphviz', class extends XTagElement {

  constructor () {
    super();
    this.graphviz = window['@hpcc-js/wasm'].graphviz;
  }

  set 'dot::attr' (dot) {
    if (!dot || dot.length === 0) return;
    this.graphviz.layout(dot, 'svg', 'dot')
      .then(svgString => {
        console.log(svgString);
        this.innerHTML = '';
        this.innerHTML = svgString;
      });
  }

});

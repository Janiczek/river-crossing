xtag.create('x-viz', class extends XTagElement {

  constructor () {
    super();
    this.viz = new Viz();
    this.imageURL = null;
  }

  //connectedCallback () {}

  '::template(true)' () {
    return this.imageURL
      ? `<img src="${this.imageURL}">`
      : '<img src="data:,">';

  }

  set 'dot::attr' (value) {
    if (!value || value.length === 0) return;
    this.viz.renderString(value, {engine: 'dot', format: 'svg'})
      .then(svgString => {
        this.imageURL = `data:image/svg+xml;charset=UTF-8,${encodeURIComponent(svgString)}`;
        this.render();
      });
  }

});

require('./index.scss')
require('./fa/js/all.js')

const mountNode = document.createElement('div')
document.body.prepend(mountNode)

const Elm = require('./Main.elm')
let app

const authToken = `zomcsparam=${getCookie('zomcscook')}`;
const authHeader = new Headers([
  ['X-ZCSRF-TOKEN', authToken]
]);

const port = chrome.runtime.connect({ name: 'broadcast' })
port.onMessage.addListener(state => {
  if (!app) {
    app = Elm.Elm.Main.init({
      node: mountNode,
      flags: authToken
    })
    app.ports.fetchInvoiceDetails.subscribe((ids) => {
      getInvoiceDetails(ids)
        .then((data) => {
          app.ports.details.send({ invoices: data.map(i => i.invoice) })
        });
    });
    return
  }

  document.getElementsByClassName('product')[0].style.marginTop
    = state.toggled
      ? '45vh'
      : '0';

  document.getElementById('zoho-helper').style.display
    = state.toggled
      ? 'block'
      : 'none';
});

function getCookie(name) {
  var value = "; " + document.cookie;
  var parts = value.split("; " + name + "=");
  if (parts.length == 2) return parts.pop().split(";").shift();
}

function getInvoiceDetails(ids) {
  return Promise.all(
    ids
      .map(id => {
        const req = new Request(
          `https://inventory.zoho.eu/api/v1/invoices/${id}?include=html&organization_id=20062851724`
          , { headers: authHeader }
        )

        return http(req);
      })
  );
}


function http(request) {
  return new Promise((resolve) => {
    fetch(request)
      .then(r => r.json())
      .then(body => {
        resolve(body);
      });
  });
};
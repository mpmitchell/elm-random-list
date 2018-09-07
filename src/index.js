import './main.css'
import { Elm } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

var savedState = localStorage.getItem('random-list-save')

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: (savedState ? JSON.parse(savedState) : null)
})

app.ports.setStorage.subscribe(function (state) {
  localStorage.setItem('random-list-save', JSON.stringify(state))
})

registerServiceWorker()

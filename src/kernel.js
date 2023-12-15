core.PopUpSnippetsModal = async (args, env) => {
    const template = await interpretate(args[0], env);
    if (document.getElementById('snippetsModal')) document.getElementById('snippetsModal').remove();

    const win = document.createElement('div');
    win.innerHTML = template;
    document.body.appendChild(win);

    attachControls(win);
}

core.ReadClipboardExtended = async (args, env) => {
  const clipboardContents = await navigator.clipboard.read();
  for (const item of clipboardContents) {

    const data = new Uint8Array(await (await item.getType(item.types[0])).arrayBuffer());
    const type = item.types[0];
    return [type, Array.from(data)];
  }
}

core.escapeLinebreaks = async (args, env) => {
  const str = await interpretate(args[0], env);
  window.testStr = str;

  return str.replaceAll('\\n', '\n').replaceAll('\\t', '\t').replaceAll('\\"', '"');
}

core.uit82b64 = async (args, env) => {
  const str = await interpretate(args[0], env);
  function bytesToBase64(bytes) {
    const binString = String.fromCodePoint(...bytes);
    return btoa(binString);
  }
  return bytesToBase64(new TextEncoder().encode(str));
}

const attachControls = (win) => {
    server.ask("JerryI`WolframJSFrontend`Snippets`Private`SnippetGet").then(async (list) => {
        autocomplete(document.getElementById("snippet-autoinput"), await interpretate(list, {}), (value)=>{
            server.socket.send('JerryI`WolframJSFrontend`Snippets`Private`SnippetPut["'+value+'"]');
        }
        , (value)=>{
          server.socket.send('JerryI`WolframJSFrontend`Snippets`Private`SnippetInfo["'+value+'"]');
        }
        , ()=>{
            win.remove();
        });
    });
}

function autocomplete(inp, arr, cbk, cbk2, destory) {
    /*the autocomplete function takes two arguments,
    the text field element and an array of possible autocompleted values:*/
    var currentFocus;
    inp.focus();
    /*execute a function when someone writes in the text field:*/
    inp.addEventListener("input", function(e) {
        var a, b, i, val = this.value;
        /*close any already open lists of autocompleted values*/
        closeAllLists();

        currentFocus = -1;
        /*create a DIV element that will contain the items (values):*/
        a = document.createElement("DIV");
        a.setAttribute("id", this.id + "autocomplete-list");
        a.setAttribute("class", "autocomplete-items");
        /*append the DIV element as a child of the autocomplete container:*/
        this.parentNode.appendChild(a);
        /*for each item in the array...*/
        for (i = 0; i < arr[0].length; i++) {
          /*check if the item starts with the same letters as the text field value:*/
          if (val.length !== 0) {
            if (arr[0][i].substr(0, val.length).toUpperCase() == val.toUpperCase()) {
              /*create a DIV element for each matching element:*/
              b = document.createElement("DIV");
              b.classList.add('autocomplete-line');
              /*make the matching letters bold:*/
              b.innerHTML = "<strong>" + arr[0][i].substr(0, val.length) + "</strong>";
              b.innerHTML += arr[0][i].substr(val.length);
              b.innerHTML += " <i> ";
              b.innerHTML += arr[1][i].substring(0, Math.min(25, arr[1][i].length));
              b.innerHTML += "...</i>";
              /*insert a input field that will hold the current array item's value:*/
              b.innerHTML += "<input type='hidden' value='" + arr[0][i] + "'>";

              b.innerHTML += "<div class=\"autocomplete-info\">?</div>";
              
              /*execute a function when someone clicks on the item value (DIV element):*/
                  b.addEventListener("click", function(e) {
                  /*insert the value for the autocomplete text field:*/
                  
                  inp.value = this.getElementsByTagName("input")[0].value;
                  /*close the list of autocompleted values,
                  (or any other open lists of autocompleted values:*/
                  if (e.target.className == "autocomplete-info") {
                    //info opener
                    done2();
                  } else {
                    //run snippet
                    done();
                  }
                  
                  closeAllLists();
                  destory();
                  
              });
              a.appendChild(b);
            }
          } else {
              /*create a DIV element for each matching element:*/
              b = document.createElement("DIV");
              /*make the matching letters bold:*/
              b.innerHTML = arr[0][i];
              b.innerHTML += " <i>";
              b.innerHTML += arr[1][i].substring(0, Math.min(45, arr[1][i].length));
              b.innerHTML += "...</i>";
              /*insert a input field that will hold the current array item's value:*/
              b.innerHTML += "<input type='hidden' value='" + arr[0][i] + "'>";
              /*execute a function when someone clicks on the item value (DIV element):*/
                  b.addEventListener("click", function(e) {
                  /*insert the value for the autocomplete text field:*/
                  inp.value = this.getElementsByTagName("input")[0].value;
                  /*close the list of autocompleted values,
                  (or any other open lists of autocompleted values:*/
                  done();
                  closeAllLists();
                  destory();
                  
              });
              a.appendChild(b);            
          }
        }
    });
    /*execute a function presses a key on the keyboard:*/
    inp.addEventListener("keydown", function(e) {
        var x = document.getElementById(this.id + "autocomplete-list");
        if (x) x = x.getElementsByTagName("div");
        if (e.keyCode == 40) {
          /*If the arrow DOWN key is pressed,
          increase the currentFocus variable:*/
          currentFocus++;
          /*and and make the current item more visible:*/
          addActive(x);
        } else if (e.keyCode == 38) { //up
          /*If the arrow UP key is pressed,
          decrease the currentFocus variable:*/
          currentFocus--;
          /*and and make the current item more visible:*/
          addActive(x);
        } else if (e.keyCode == 13) {
          /*If the ENTER key is pressed, prevent the form from being submitted,*/
          e.preventDefault();
          if (currentFocus > -1) {
            /*and simulate a click on the "active" item:*/
            if (x) x[currentFocus].click();
          }
        } else if (e.keyCode == 27) {
            closeAllLists(undefined);
            destory();
        }
    });
    function addActive(x) {
      /*a function to classify an item as "active":*/
      if (!x) return false;
      /*start by removing the "active" class on all items:*/
      removeActive(x);
      if (currentFocus >= x.length) currentFocus = 0;
      if (currentFocus < 0) currentFocus = (x.length - 1);
      /*add class "autocomplete-active":*/
      x[currentFocus].classList.add("autocomplete-active");
    }
    function removeActive(x) {
      /*a function to remove the "active" class from all autocomplete items:*/
      for (var i = 0; i < x.length; i++) {
        x[i].classList.remove("autocomplete-active");
      }
    }

    function done() {
        cbk(inp.value);
    }

    function done2() {
      cbk2(inp.value);
  }

    function closeAllLists(elmnt) {
      /*close all autocomplete lists in the document,
      except the one passed as an argument:*/
      var x = document.getElementsByClassName("autocomplete-items");
      for (var i = 0; i < x.length; i++) {
        if (elmnt != x[i] && elmnt != inp) {
        x[i].parentNode.removeChild(x[i]);
      }

      
    }
  }
  const oclk = (e) => {
    closeAllLists(e.target);
    console.log('clicklick');
    destory();
    document.removeEventListener("click", oclk);
  }
  /*execute a function when someone clicks in the document:*/
  document.addEventListener("click", oclk);
}
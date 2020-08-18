const rp = require("request-promise");
const $ = require("cheerio");
const fs = require("fs");

var stanovanja = [];
var obj = {};

var base_url = "https://www.nepremicnine.net";
var vic_url = base_url+"/oglasi-oddaja/ljubljana-mesto/ljubljana-vic-rudnik/stanovanje/";
var bezigrad_url = base_url+"/oglasi-oddaja/ljubljana-mesto/ljubljana-bezigrad/stanovanje/";

for(var i = 1; i <= 20; i++) {
  // pazi url
  rp(vic_url+"/"+i+"/").then(html => {
    var seznam = $("div.seznam div.oglas_container", html);
    seznam.each((j, element) => {
      obj = {};
      var faulty = false;

      // kot id
      obj.ime = $("span.title", element).text();

      // link
      obj.link = base_url+$("a", element).attr("href");
      if(!obj.link.includes("/oglasi-oddaja/")) {
        faulty = true;
      }

      // nadstropje in leto
      try {
        var podatki = $('span.atribut', element).text().split(", ");

        var nadstropje = podatki[0].split(" ")[1].split("/");
        obj.nadstropje = nadstropje[0];
        if(nadstropje[1]) {
          obj.toCheck = false;
          obj.vsaNadstropja = nadstropje[1];
        } else {
          obj.toCheck = true;
          obj.vsaNadstropja = "-";
        }

        obj.letoGradnje = podatki[1].split(" ")[1].substring(0, 4);
      } catch(err) {
        faulty = true;
      }

      // povrsina
      obj.povrsina = $('span.velikost', element).text().split(" ")[0];

      // najemnina
      obj.cena = $('span.cena', element).text().split(" ")[0];

      if(!faulty) {
        stanovanja.push(obj);
      }
    });

    // pazi ime
    fs.writeFile("stanovanja.json", JSON.stringify(stanovanja), (err) => {
      if(err) {
        console.log(err);
      }
    });
  }).catch(error => {
    console.log(error);
  });
}

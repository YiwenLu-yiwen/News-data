import define1 from "./inputs.js";

export default function define(runtime, observer) {
  const main = runtime.module();
  main.variable(observer()).define(["md"], function(md){return(
md`## Sentiment Analysis`
)});
  main.variable("d3").define("d3", ["require"], function(require){return(
require("d3")
)});
  main.variable("news").define("news", ["d3"], function(d3){return(
d3.csv('./NY Times.csv')
)});
  main.variable("nlp").define("nlp", ["require"], function(require){return(
require('compromise')
)});
  main.variable("info").define("info", ["news"], function(news){return(
function(name){
 var list = []
 for (var i = 0; i < news.length; i++){
       list.push(news[i][name])
      }
return list}
)});
  main.variable("short_description").define("short_description", ["info"], function(info){return(
info('short_description')
)});
  main.variable("short_description_nlp").define("short_description_nlp", ["nlp","short_description"], function(nlp,short_description){return(
nlp(short_description).normalize().sentences()
)});
  main.variable("positive_negative_verbs").define("positive_negative_verbs", ["short_description_nlp"], function(short_description_nlp){return(
short_description_nlp.sentences().map(function(m){
    return{'sentence':m.out(), //Extract the standarized sentences as pure text.
           'negative_verbs': m.verbs().isNegative().length, //.verbs().isNegative().length Count all the negative verbs in the sentence
           'positive_verbs':m.verbs().isPositive().length //.verbs().isPositive().length Count all positive verbs in the sentence
          }})
)});
  main.variable("sentence_sentiment").define("sentence_sentiment", ["short_description_nlp"], function(short_description_nlp){return(
short_description_nlp.sentences().map(function(m){
    var sentiment = 0 
    if (m.verbs().isNegative().length != 0)
      sentiment = -1;
    else if(m.verbs().isPositive().length > 0)
      sentiment = 1;
    return{'sentence':m.out(), //Extract the standarized sentences as pure text.
           'sentiment': sentiment //Assign sentiment values to sentences.
          }})
)});
  const child1 = runtime.module(define1);
  main.import("select", child1);
  main.variable(observer("viewof method_colors_selection")).define("viewof method_colors_selection", ["select"], function(select)
{ 
  const bds = select({
      title: "Please choose a method to visualize the sentiment features",
      options: [{label:"Colord Rectangles", value:"rect"},
                {label:"Colored Sentences", value:"text"}],
      value:"text"
    });
    return bds;
}
);
  main.variable("method_colors_selection").define("method_colors_selection", ["Generators", "viewof method_colors_selection"], (G, _) => G.input(_));
  main.variable(observer()).define(["html"], function(html){return(
html `<svg id="sentiment-bars"></svg>`
)});
  main.variable(("createSentimentChart")).define("createSentimentChart", ["d3","width"], function(d3,width){return(
function(sentences, method) {
    document.querySelector('#sentiment-bars').innerHTML = ""; //Empty the SVG element from any previous elements.
  const format = d3.format(".3f");
  const margin = ({top: 30, right: 0, bottom: 10, left: 30});
  const svg = d3.select("#sentiment-bars");

  const x = d3.scaleLinear() //Specfiy the x scale
    .domain([0, 10])
    .range([margin.left, width - margin.right]);
  const colors = ['red','grey','green']; //Specfiy the colors we will use for our features
  const y_ticks = [...Array(sentences.length).keys()]; //generate line numbers to be the ticks of the y scale
  
  const xAxis = g => g //Initialize the xAxis and attach the x scale to it.
    .attr("transform", `translate(0,${margin.top})`)
    .call(d3.axisTop(x).ticks(width / 80))
    .call(g => g.select(".domain").remove());
 

 if(method === "rect") { //Create the colored rectangles version
   //Specify the height of the chart which will also determing the height of each of the rectangles
  const height = sentences.length * 5 + margin.top + margin.bottom;
  const y = d3.scaleBand() //initialize the y scale
    .domain(sentences.map((d,i) => y_ticks[i]))
    .range([margin.top, height - margin.bottom])
    .padding(0.1);
   svg.append("g") //Create a rectangle for every sentence in the dataset
    .selectAll("rect")
    .data(sentences)
    .join("rect")
      .attr("x", x(0))
      .attr("y", (d,i) => y(i))
      .attr("fill",d => colors[d.sentiment+1])
      .attr("width", d => "150")
      .attr("height", y.bandwidth())
      .append("title")
        .text(function(d) {
            return d.sentence;
        });
   svg.attr("width",width) //Specify the width and height of the chart
    .attr("height",height);

  }
  else if (method === "text"){ //Create the colored text version
   const height = sentences.length * 12 + margin.top + margin.bottom;
   const y = d3.scaleBand() //initialize the y scale
    .domain(sentences.map((d,i) => y_ticks[i]))
    .range([margin.top, height - margin.bottom])
    .padding(0.1); 
   const yAxis = g => g //initialize the y axis and attach the y scale to it
    .attr("transform", `translate(${margin.left},0)`)
    .call(d3.axisLeft(y).tickSizeOuter(0)); 
   svg.append("g") 
      .style("font", "10px sans-serif")
    .selectAll("text")
    .data(sentences)
    .join("text")
      .attr("x", d => x(0)+5) //The sentence position on the x axis
      .attr("y", (d,i) => y(i) + y.bandwidth() / 2) // The sentence position on the y axis
      .attr("dy", "0.35em")
      .attr("fill",d => colors[d.sentiment+1])
      .text(d => d.sentence); 
    
   svg.append("g") // Add the line numbers next to the lines
      .call(yAxis);
    
   svg.attr("width",width) //Specify the width and height of the chart
    .attr("height",height);

  }

  return svg.node();
}
)});
  main.variable(observer()).define(["createSentimentChart","sentence_sentiment","method_colors_selection"], function(createSentimentChart,sentence_sentiment,method_colors_selection){return(
createSentimentChart(sentence_sentiment, method_colors_selection)
)});
  return main;
}

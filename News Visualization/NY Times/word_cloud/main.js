import define1 from "./saving-svg.js";
import define2 from "./inputs.js";

export default function define(runtime, observer) {
  const main = runtime.module();
  main.variable(observer()).define(["md"], function(md){return(
md`## Word Cloud `
)});
  main.variable(("d3")).define("d3", ["require"], function(require){return(
require("d3")
)});
  main.variable(("nlp")).define("nlp", ["require"], function(require){return(
require('compromise')
)});
  main.variable(("d3cloud")).define("d3cloud", ["require"], function(require){return(
require('d3-cloud')
)});
  const child1 = runtime.module(define1);
  main.import("rasterize", child1);
  main.import("serialize", child1);
  main.variable(("news")).define("news", ["d3"], function(d3){return(
d3.csv('./NY Times.csv')
)});
  main.variable(("info")).define("info", ["news"], function(news){return(
function(name){
 var list = []
 for (var i = 0; i < news.length; i++){
       list.push(news[i][name])
      }
return list}
)});
  main.variable(("short_description")).define("short_description", ["info"], function(info){return(
info('short_description')
)});
  main.variable(("short_description_nlp")).define("short_description_nlp", ["nlp","short_description"], function(nlp,short_description){return(
nlp(short_description).normalize().sentences()
)});
  main.variable(("all_tokens")).define("all_tokens", ["short_description_nlp"], function(short_description_nlp){return(
short_description_nlp.terms().out('frequency').slice(0,500)
)});
  main.variable(("stop_words")).define("stop_words", function(){return(
"(a|again|couldn't|further|but|still|thence|across|never|fill|don|are|beforehand|serious|you'll|mustn't|ever|ourselves|you've|of|else|it's|about|elsewhere|others|am|when|empty|thereby|now|ltd|very|nowhere|wasn|didn|whose|last|hadn|under|ll|get|what|de|few|as|ie|below|without|me|four|won|among|less|becomes|everywhere|would|an|least|into|whole|however|go|wouldn|whereas|eleven|nobody|there|aren't|bottom|beyond|twenty|first|not|didn't|before|alone|doesn|at|throughout|while|through|with|via|whatever|nor|ours|both|around|made|against|shouldn't|upon|can|must|shouldn|even|fifty|up|his|they|someone|we|also|be|namely|move|amoungst|many|none|another|whenever|any|those|between|hundred|our|himself|may|much|i|every|inc|he|show|hasnt|interest|ma|haven|sometime|if|thick|fifteen|than|see|therein|three|that'll|after|whoever|seems|until|too|anyone|moreover|do|mostly|hereupon|back|from|these|always|ve|whereby|former|along|amongst|rather|shan|this|should|nevertheless|here|having|once|full|yours|yourselves|often|because|the|mightn't|hadn't|several|couldnt|seemed|con|haven't|whether|two|seeming|does|she's|by|so|toward|perhaps|to|just|wouldn't|for|doing|anything|don't|cannot|six|who|will|more|has|did|or|somewhere|fire|is|bill|thus|please|weren|latterly|sometimes|twelve|whereafter|already|side|only|eg|was|put|mill|hence|such|thereafter|eight|third|had|all|indeed|towards|theirs|detail|wherever|its|aren|yet|next|meanwhile|itself|should've|everything|since|hasn|mustn|seem|whereupon|myself|it|sincere|whence|except|whither|being|anyway|then|wasn't|everyone|noone|although|cant|over|part|mine|him|hasn't|otherwise|above|out|thereupon|us|them|wherein|yourself|been|becoming|find|you|during|five|take|somehow|ain|something|neither|onto|might|their|thru|system|how|needn|within|forty|name|together|whom|same|top|why|have|isn't|front|one|hers|down|formerly|themselves|mightn|found|and|give|where|beside|her|own|amount|could|anywhere|were|weren't|which|became|due|in|isn|hereafter|my|she|anyhow|your|couldn|un|some|nine|per|latter|either|describe|no|won't|almost|on|re|afterwards|hereby|well|keep|herself|etc|herein|nothing|needn't|enough|each|other|most|doesn't|become|that|sixty|besides|off|though|therefore|done|shan't|co|thin|you'd|call|ten|you're|cry|behind|0|1|2|3|4|5|6|7|8|9|*)"
)});
  main.variable(("filtered_terms")).define("filtered_terms", ["nlp","short_description","stop_words"], function(nlp,short_description,stop_words){return(
nlp(short_description).normalize().sentences().delete(stop_words).terms().out('frequency').slice(0,200)
)});
  main.variable(("people")).define("people", ["short_description_nlp"], function(short_description_nlp){return(
short_description_nlp.people().out('frequency').slice(0, 200)
)});
  main.variable(("places")).define("places", ["short_description_nlp"], function(short_description_nlp){return(
short_description_nlp.places().out('frequency').slice(0, 200)
)});
  const child2 = runtime.module(define2);
  main.import("select", child2);
  main.variable(observer("viewof cloud_dataset_selection")).define("viewof cloud_dataset_selection", ["select"], function(select)
{ 
  const cds = select({
      title: "Please choose a dataset to visualize on the word cloud:",
      options: [{label:"Unfiltered Words", value:0},
                {label:"Filtered Words", value:1},
                {label:"Places", value:2},
                {label:"People", value:3}],
      value:0
    });
    return cds;
}
);
  main.variable(("cloud_dataset_selection")).define("cloud_dataset_selection", ["Generators", "viewof cloud_dataset_selection"], (G, _) => G.input(_));
  main.variable(("new_tokens")).define("new_tokens", ["toWords","all_tokens"], function(toWords,all_tokens){return(
toWords(all_tokens)
)});
  main.variable(observer("wordCloud")).define("wordCloud", ["createWordCloudSvg","lists","cloud_dataset_selection"], function(createWordCloudSvg,lists,cloud_dataset_selection){return(
createWordCloudSvg(lists[cloud_dataset_selection])
)});
  main.variable(observer("download")).define("download", ["downloadWordCloudSvg","wordCloud"], function(downloadWordCloudSvg,wordCloud){return(
downloadWordCloudSvg('word-cloud', wordCloud)
)});
  main.variable(("wordColors")).define("wordColors", ["d3"], function(d3){return(
d3.scaleSequential(d3.interpolateRainbow)
)});
  main.variable(("rotateWord")).define("rotateWord", function(){return(
function () { 
  return ~~(Math.random() * 4) * 45 - 45; 
}
)});
  main.variable(("frequencyToSize")).define("frequencyToSize", function(){return(
function (frequency) {
  return Math.sqrt(frequency);
}
)});
  main.variable(("fontFamilies")).define("fontFamilies", function(){return(
['Corben', 'Pacifico', 'impact']
)});
  main.variable(("baseFont")).define("baseFont", ["fontFamilies"], function(fontFamilies){return(
function (d) {
  return fontFamilies[~~(Math.random() * fontFamilies.length)]
}
)});
  main.define("initial cloudScale", function(){return(
1
)});
  main.variable(("mutable cloudScale")).define("mutable cloudScale", ["Mutable", "initial cloudScale"], (M, _) => new M(_));
  main.variable(("cloudScale")).define("cloudScale", ["mutable cloudScale"], _ => _.generator);
  main.variable(("cloudConfig")).define("cloudConfig", ["width"], function(width){return(
{
  minFontSize: 10,
  maxFontSize: 80,
  height: width/2,
  padding: 1,
}
)});
  main.variable(("fontSize")).define("fontSize", ["toWords","lists","cloud_dataset_selection","frequencyToSize","cloudConfig","width","mutable cloudScale"], function(toWords,lists,cloud_dataset_selection,frequencyToSize,cloudConfig,width,$0)
{
  let words = toWords(lists[cloud_dataset_selection])
  let totalArea = 0;
  let minSize = frequencyToSize(words[words.length-1].freq);
  let maxSize = frequencyToSize(words[0].freq);
  for (let w of words) {
    let size = frequencyToSize(w.freq);
    let fontSize = cloudConfig.minFontSize + 
      (cloudConfig.maxFontSize - cloudConfig.minFontSize) * ((size-minSize) / (maxSize-minSize));
    totalArea += (w.text.length * 0.6 + cloudConfig.padding * 2) * fontSize * (fontSize + cloudConfig.padding * 2);
  }
  let s = Math.sqrt(width * cloudConfig.height/totalArea);
  $0.value = s;
  return function (w) {
    return s * (cloudConfig.minFontSize + 
        (cloudConfig.maxFontSize - cloudConfig.minFontSize) * ((frequencyToSize(w.freq) - minSize) / (maxSize - minSize))
      );
  }
}
);
  main.variable(observer("createWordCloudSvg")).define("createWordCloudSvg", ["toWords","d3cloud","width","cloudConfig","cloudScale","rotateWord","baseFont","fontSize","DOM","d3","wordColors"], function(toWords,d3cloud,width,cloudConfig,cloudScale,rotateWord,baseFont,fontSize,DOM,d3,wordColors){return(
function createWordCloudSvg(tokens) {
  let words = toWords(tokens)
  var layout = d3cloud()
    .size([width, width * 9/16]) 
    .words(words)
    .padding(cloudConfig.padding * cloudScale)
    .rotate(rotateWord)
    .font(baseFont)
    .fontSize(fontSize)
    .on('word', addWord);

  const svg = DOM.svg(layout.size()[0], layout.size()[1]); // width, height
  const group = d3.select(svg).append('g')
    //.attr("transform", "translate(" + layout.size()[0] / 2 + "," + layout.size()[1] / 2 + ")")
  
  function addWord (word) {
    const text = group.append('text');
    text.style('font-size', '2px')
      .style('font-family', word.font)
      .style('fill', wordColors(Math.random()))
      .style('cursor', 'pointer')
      .attr('text-anchor', 'middle')
      .attr('transform', `translate(${[word.x, word.y]})rotate(${word.rotate})`)
      .text(word.text)
      //.transition()
      //.duration(1500)
      //.ease(d3.easeLinear)
      .style('font-size', `${word.size}px`);
    text.append('title').text(`${word.text} (${word.count})`); // toolitp
  }
  
  layout.start();
  return svg;
}
)});
  main.variable(("downloadWordCloudSvg")).define("downloadWordCloudSvg", ["html","DOM","serialize"], function(html,DOM,serialize){return(
function downloadWordCloudSvg(fileName, svg) {
  return html `${DOM.download(serialize(svg), `${fileName}.svg`, "Save SVG")}`;
}
)});
  main.variable(("lists")).define("lists", ["all_tokens","filtered_terms","places","people"], function(all_tokens,filtered_terms,places,people){return(
[all_tokens, filtered_terms, places, people]
)});
  main.variable(("toWords")).define("toWords", function(){return(
function toWords (terms) {
  return terms.map(term => ({
    text: term.normal,
    count: term.count,
    freq: term.percent/100
  }));
}
)});
  main.variable(observer()).define(["md"], function(md){return(
md`## Word Frequency`
)});
  main.variable(observer("viewof barchart_dataset_selection")).define("viewof barchart_dataset_selection", ["select"], function(select)
{ 
  const bds = select({
      title: "Please choose a dataset to visualize on the barchart:",
      options: [{label:"All Words", value:0},
                {label:"Filtered Words", value:1},
                {label:"Places", value:2},
                {label:"People", value:3}],
      value:0
    });
    return bds;
}
);
  main.variable(("barchart_dataset_selection")).define("barchart_dataset_selection", ["Generators", "viewof barchart_dataset_selection"], (G, _) => G.input(_));
  main.variable(observer()).define(["html"], function(html){return(
html `<svg id="frequency-barchart"></svg>`
)});
  main.variable(observer("createBarChart")).define("createBarChart", ["d3","width"], function(d3,width){return(
function(words) {
    document.querySelector('#frequency-barchart').innerHTML = ""; //Empty the SVG element from any previous elements.
  //Adapted 
  const format = d3.format(".0f"); //Specify how many decimal places to show on the barchart label, here it is 0 because we are using integer counts.
  const margin = ({top: 30, right: 0, bottom: 10, left: 55}); //specify the margins of the graph
  const height = words.length * 25 + margin.top + margin.bottom; //specify the height of the chart which will also determine the width of every bar (here the width of the bar is the height because it is a horizontal chart) 
  const svg = d3.select("#frequency-barchart"); //Select the SVG element by the unique ID we gave it earlier.
  
  svg.attr("width",width) //Specify the width and height of the chart
    .attr("height",height);

  const x = d3.scaleLinear() //Create the x scale
    .domain([0, d3.max(words, d => d.count)])
    .range([margin.left, width - margin.right]);
  
  const y = d3.scaleBand() //Ctrate the y scale
    .domain(words.map(d => d.normal))
    .range([margin.top, height - margin.bottom])
    .padding(0.1);
  
  const xAxis = g => g //create the x Axis and attach the x scale to it
    .attr("transform", `translate(0,${margin.top})`)
    .call(d3.axisTop(x).ticks(width / 80))
    .call(g => g.select(".domain").remove());
  const yAxis = g => g //create the y Axis and attach the y scale to it
    .attr("transform", `translate(${margin.left},0)`)
    .call(d3.axisLeft(y).tickSizeOuter(0));
  
  svg.append("g") //Create each bar in the bar chart
      .attr("fill", "steelblue")
    .selectAll("rect")
    .data(words)
    .join("rect")
      .attr("x", x(0))
      .attr("y", d => y(d.normal)) // The word 
      .attr("width", d => x(d.count) - x(0)) // The frequency of the word
      .attr("height", y.bandwidth())
			.on("mouseover", function() {
			   		d3.select(this)
			   			.attr("fill", "orange");
			   })
			.on("mouseout", function(d) {
				   d3.select(this)
						.attr("fill", "rgb(0, 0, " + (d * 10) + ")");
			   }); // Get the width of the Band in the Y scale
  
  svg.append("g") // Create the count label on the top of each bar chart
      .attr("fill", "white")
      .attr("text-anchor", "end")
      .style("font", "12px sans-serif")
    .selectAll("text")
    .data(words)
    .join("text")
      .attr("x", d => x(d.count) - 4) //The label position on the x axis
      .attr("y", d => y(d.normal) + y.bandwidth() / 2) // The label position on the y axis
      .attr("dy", "0.35em")
      .text(d => format(d.count)); 
  
  svg.append("g") //Add the x axis to the barchart
      .call(xAxis);
  
  svg.append("g") //Add the y axis to the barchart
      .call(yAxis);
  
  return svg.node();
}
)});
  main.variable(observer()).define(["createBarChart","lists","barchart_dataset_selection"], function(createBarChart,lists,barchart_dataset_selection){return(
createBarChart(lists[barchart_dataset_selection])
)});
  return main;
}

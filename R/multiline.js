// !preview r2d3 data=tibble(
//    date = as.Date(c("2020-01-01","2020-01-01","2020-01-01","2020-01-10","2020-01-30","2020-01-20")),
//    value = c(1,2,3,10,20,30),
//    name = c("val1","val2","val3")
// )
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);

svg.append("g").call(xAxis);
svg.append("g").call(yAxis);

data = {
  const data = d3.tsvParse(await FileAttachment("unemployment.tsv").text());
  const columns = data.columns.slice(1);
  return {
    y: "% Unemployment",
    series: data.map(d => ({
      name: d.name.replace(/, ([\w-]+).*/, " $1"),
      values: columns.map(k => +d[k])
    })),
    dates: columns.map(d3.utcParse("%Y-%m"))
  };
}


const path = svg.append("g")
    .attr("fill", "none")
    .attr("stroke", "steelblue")
    .attr("stroke-width", 1.5)
    .attr("stroke-linejoin", "round")
    .attr("stroke-linecap", "round")
  .selectAll("path")
  .data(data)
  .join("path")
    .style("mix-blend-mode", "multiply")
    .attr("d", d => line(d.value));

svg.call(hover, path);
  

function hover(svg, path) {
  if ("ontouchstart" in document) svg
      .style("-webkit-tap-highlight-color", "transparent")
      .on("touchmove", moved)
      .on("touchstart", entered)
      .on("touchend", left)
  else svg
      .on("mousemove", moved)
      .on("mouseenter", entered)
      .on("mouseleave", left);

  const dot = svg.append("g")
      .attr("display", "none");

  dot.append("circle")
      .attr("r", 2.5);

  dot.append("text")
      .attr("font-family", "sans-serif")
      .attr("font-size", 10)
      .attr("text-anchor", "middle")
      .attr("y", -8);

  function moved() {
    d3.event.preventDefault();
    const mouse = d3.mouse(this);
    const xm = x.invert(mouse[0]);
    const ym = y.invert(mouse[1]);
    const i1 = d3.bisectLeft(data.dates, xm, 1);
    const i0 = i1 - 1;
    const i = xm - data.dates[i0] > data.dates[i1] - xm ? i1 : i0;
    const s = d3.least(data.series, d => Math.abs(d.values[i] - ym));
    path.attr("stroke", d => d === s ? null : "#ddd").filter(d => d === s).raise();
    dot.attr("transform", `translate(${x(data.dates[i])},${y(s.values[i])})`);
    dot.select("text").text(s.name);
  }

  function entered() {
    path.style("mix-blend-mode", null).attr("stroke", "#ddd");
    dot.attr("display", null);
  }

  function left() {
    path.style("mix-blend-mode", "multiply").attr("stroke", null);
    dot.attr("display", "none");
  }
  
  
}  

line = d3.line()
    .defined(d => !isNaN(d))
    .x((d, i) => x(data.date[i]))
    .y(d => y(d))


x = d3.scaleUtc()
    .domain(d3.extent(data.date))
    .range([margin.left, width - margin.right])

y = d3.scaleLinear()
  .domain([0, d3.max(data.value)]).nice()
  .range([height - margin.bottom, margin.top])

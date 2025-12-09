<!-- Load Plotly -->
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

<div id="sankeyDiv" style="width:100%; height:700px;"></div>

<script>

// Correct RAW GitHub URLs
const nodesURL = "https://raw.githubusercontent.com/saeed-saffari/CPHSC/main/stories/hospital_food/nodes_sankey.json";
const linksURL = "https://raw.githubusercontent.com/saeed-saffari/CPHSC/main/stories/hospital_food/links_sankey.json";

Promise.all([
    fetch(nodesURL).then(res => res.json()),
    fetch(linksURL).then(res => res.json())
]).then(([nodes, links]) => {

    const data = {
        type: "sankey",
        arrangement: "snap",
        node: {
            pad: 15,
            thickness: 20,
            label: nodes,
            line: { color: "gray", width: 0.5 }
        },
        link: {
            source: links.map(d => d.IDsource),
            target: links.map(d => d.IDtarget),
            value: links.map(d => d.value),
            color: "rgba(200,200,200,0.6)"
        }
    };

    Plotly.newPlot("sankeyDiv", [data], {
        title: "Total Hospital Food GHG Emissions",
        font: { size: 14 }
    });

});
</script>

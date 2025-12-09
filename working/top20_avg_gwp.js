<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

<div id="avgPlot" style="width:100%; height:600px;"></div>

<script>
fetch("https://raw.githubusercontent.com/saeed-saffari/CPHSC/main/stories/hospital_food/top20_avg_plot.json")
  .then(res => res.json())
  .then(fig => {
      Plotly.newPlot("avgPlot", fig.data, fig.layout);
  })
  .catch(err => console.error("Error loading JSON:", err));
</script>




<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

<!-- Dropdown -->
<select id="foodSelect" multiple size="6" style="margin-bottom:20px; width:250px;">
  <!-- dynamically populated -->
</select>

<div id="avgPlot" style="width:100%; height:600px;"></div>

<script>
const dataURL = "https://raw.githubusercontent.com/saeed-saffari/CPHSC/main/stories/hospital_food/top20_avg_gwp.json";

fetch(dataURL)
  .then(res => res.json())
  .then(data => {

      const foods = data.food_items;
      const values = data.values;
      const baseColors = data.colors;

      // Populate dropdown
      const dropdown = document.getElementById("foodSelect");
      foods.forEach((f, i) => {
          const opt = document.createElement("option");
          opt.value = i;
          opt.textContent = f;
          dropdown.appendChild(opt);
      });

      // Update plot based on selection
      function updatePlot() {
          const selected = Array.from(dropdown.selectedOptions).map(o => parseInt(o.value));

          let x, y, colors;

          if (selected.length === 0) {
              // Show all if nothing selected
              x = foods;
              y = values;
              colors = baseColors;
          } else {
              x = selected.map(i => foods[i]);
              y = selected.map(i => values[i]);
              colors = selected.map(i => baseColors[i]);
          }

          const trace = {
              type: "bar",
              x: x,
              y: y,
              marker: { color: colors, line: { color: "black", width: 0.6 }},
              hovertemplate: "%{x}<br>%{y} kg CO2e<extra></extra>"
          };

          const layout = {
              margin: {l: 60, r: 20, t: 20, b: 100},
              yaxis: { title: "Average kg CO2e" },
              xaxis: { tickangle: 45 },
              plot_bgcolor: "white",
              paper_bgcolor: "white",
              font: { family: "Times New Roman", size: 14 }
          };

          Plotly.newPlot("avgPlot", [trace], layout);
      }

      // Initial full plot
      updatePlot();

      // Trigger update whenever user changes selection
      dropdown.addEventListener("change", updatePlot);
  });
</script>
$(function(){ // on dom ready

$.getJSON('/esi/http_handler:get_graph', function(data) {
  var cytoscape_nodes = [];
  var cytoscape_edges = [];
  data.edge_statuses.forEach(function (elem, i, arr) {
    if (elem.type == "local") {
      cytoscape_nodes.push({ data: { id: elem.server, name: elem.server } });
      cytoscape_edges.push({ data: { down:elem.down, source: elem.server, target: elem.server, id: "loop+" + elem.server } });
    }
    if (elem.type == "s2s")
      cytoscape_edges.push({ data: { down:elem.down, source: elem.servers[0], target: elem.servers[1], id: elem.servers[0] + elem.servers[1] } });
  });

  cy = cytoscape({
    container: document.getElementById("cy"),
    style: cytoscape.stylesheet()
      .selector('edge') .css({
          'width': 2,
        })
      .selector('node')
        .css({
          'content': 'data(name)',
          'text-valign': 'center',
          'color': 'white',
          'text-outline-width': 2,
          'text-outline-color': '#888'
        })
      .selector('edge[?down]')
        .css({
          'line-color': 'red',
          'line-style': 'dashed'
        })
      .selector('edge[!down]')
        .css({
          'line-color': 'green',
          'line-style': 'solid'
        }),
    elements: {
      nodes: cytoscape_nodes,
      edges: cytoscape_edges
    },
    layout: {
      name: 'circle',
      padding: 50
    }
  });

  setInterval(function() {
    $.ajax({
      url: '/esi/http_handler:get_graph',
      dataType: 'json',
      error: function() {
        $("#error").show();
      },
      success: function(data) {
        $("#error").hide();
        data.edge_statuses.forEach(function (elem, i, arr) {
          if (elem.type == "local")
            cy.getElementById("loop+" + elem.server).data("down", elem.down);
          if (elem.type == "s2s") {
            cy.getElementById(elem.servers[0] + elem.servers[1]).data("down", elem.down);
          }
        });
      }
    });
  }, 5000);
});

}); // on dom ready

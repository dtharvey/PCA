# server file for PCA learning module

shinyServer(function(input, output, session){
  
  output$plotlyactivity1a = renderPlotly({
    axis_width = 8L
    rdata = as.data.frame(rotdata)
    x_axis = data.frame(xpc1, ypc1, zpc1)
    y_axis = data.frame(xpc2, ypc2, zpc2)
    z_axis = data.frame(xpc3, ypc3, zpc3)
    fig = plot_ly(
      name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
      marker = list(size = 3, color = okabe_ito[3])
    ) %>%
      add_markers() %>%
      add_trace(name = "x axis", data = x_axis, x = ~xpc1, y = ~ypc1, z = ~zpc1, 
                mode = "lines", type = "scatter3d", inherit = FALSE, 
                line = list(width = axis_width, color = okabe_ito[2])) %>%
      add_trace(name = "y axis", data = y_axis, x = ~xpc2, y = ~ypc2, z = ~zpc2, 
                mode = "lines", type = "scatter3d", inherit = FALSE, 
                line = list(width = axis_width, color = okabe_ito[4])) %>%
      add_trace(name = "z axis", data = z_axis, x = ~xpc3, y = ~ypc3, z = ~zpc3, 
                mode = "lines", type = "scatter3d", inherit = FALSE, 
                line = list(width = axis_width, color = okabe_ito[8])) %>%
      layout(scene = list(
        xaxis = list(title = "original x-axis"),
        yaxis = list(title = "original y-axis"),
        zaxis = list(title = "original z-axis")
      ))
    fig
  })
  
  output$plotlyactivity2a = renderPlotly({
    axis_width = 8L
    rdata = as.data.frame(rotdata)
    fig = plot_ly(
      name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
      marker = list(size = 3, color = okabe_ito[3])
    ) %>%
      add_markers() %>%
      add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, 
                mode = "lines", type = "scatter3d", inherit = FALSE, 
                line = list(color = okabe_ito[1], width = axis_width),
                visible = "legendonly") %>%
      add_trace(name = "PC2", data = pc2, x = ~xrot, y = ~yrot, z = ~zrot, 
                mode = "lines", type = "scatter3d", inherit = FALSE, 
                line = list(color = okabe_ito[4], width = axis_width),
                visible = "legendonly") %>%
      add_trace(name = "PC3", data = pc3, x = ~xrot, y = ~yrot, z = ~zrot, 
                mode = "lines", type = "scatter3d", inherit = FALSE, 
                line = list(color = okabe_ito[7], width = axis_width),
                visible = "legendonly") %>%
      layout(scene = list(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        zaxis = list(title = "")
      ))
    fig
  })
  
  output$activity3a = renderPlot({
    rot_raw_data = rot_axes(file = raw_data, angle = input$rotangle)
    plot_rot_axes(file = rot_raw_data, 
                  show_rotated = TRUE,
                  show_projections = TRUE,
                  show_full_legend = FALSE,
                  show_full_axes_legend = FALSE,
                  show_title = FALSE,
                  show_loadings = FALSE,
                  show_scores = TRUE,
                  show_simple_legend = TRUE)
  })
  
  output$activity4a = renderPlot({
    rot_raw_data = rot_axes(file = raw_data, angle = input$rotangle_load)
    plot_rot_axes(file = rot_raw_data, 
                  show_rotated = TRUE,
                  show_projections = TRUE,
                  show_full_legend = TRUE,
                  show_full_axes_legend = FALSE,
                  show_title = FALSE,
                  show_loadings = FALSE,
                  show_scores = FALSE,
                  show_simple_legend = FALSE)
  })
  
})



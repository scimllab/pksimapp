#' @import shiny
#' @import bslib
#' @export
app_ui <- function() {
  page_navbar(
    id = "main_navbar",
    title = "Clinical Pk Simulation",
    theme = bs_theme(version = 5, bootswatch = "minty"),
    bg = "#2C3E50",
    
    header = tags$head(
      tags$script(HTML("
        window.addEventListener('beforeunload', function (e) {
          Shiny.setInputValue('browser_closed', true);
        });
      "))
    ),
    
    
    # ============================================================================
    # TAB : PREDICTIVE TRIAL SIMULATION (ADVANCED MULTI-ENGINE ARCHITECTURE)
    # ============================================================================
    nav_panel("Simulation Workspace",
              navset_pill_list(
                id = "simulation_pills",
                widths = c(3, 9),
                nav_panel("🔮 Unified Forecast Workspace",
                          layout_columns(
                            col_widths = c(4, 8),
                            
                            # Left Panel: Ingest, Target Engine Routing and Controls
                            card(
                              card_header("Simulation Dispatch Pad"),
                              # Find your radioButtons("sim_model_source_layer", ...) block and update the choices list:
                              radioButtons("sim_model_source_layer", "1. Model Source Layer:",
                                           choices = list(
                                             "Use Active Session Ledger Runs" = "session_ledger",
                                             "Pull Archived Run from SharePoint" = "sharepoint_models",
                                             "Upload Run File from Desktop" = "desktop_upload",
                                             "⚡ Re-import Full Zip Run Folder Package" = "zip_reimport" # NEW INTERFACE NODE
                                           ), selected = "session_ledger"),
                              hr(),
                              uiOutput("sim_model_selector_wrapper"),
                              
                              hr(),
                              # Target Simulation Engine Radio Selection
                              radioButtons("target_sim_engine", "2. Target Simulation Engine:",
                                           choices = list(
                                             "NONMEM Engine (via PsN Command)" = "nonmem",
                                             "mrgsolve Engine (R-Based ODE)" = "mrgsolve",
                                             "nlmixr2 Engine (Open-Source RxODE)" = "nlmixr2"
                                           ), selected = "nonmem"),
                              
                              actionButton("translate_and_compile_model", "🤖 Translate Structure & Inject Parameters", class = "btn-info w-100 mb-3"),
                              
                              hr(),
                              sliderInput("sim_dose_mg", "Target Dosing Profile (mg):", min = 50, max = 1000, value = 200, step = 50),
                              sliderInput("sim_n_subjects", "Virtual Cohort Size (N):", min = 10, max = 200, value = 50, step = 10),
                              actionButton("run_nonmem_simulation", "🔮 Launch Active Simulation Engine", class = "btn-danger w-100"),
                              # ⚡ NEW: COMPREHENSIVE ARCHIVE RUN FOLDER EXPORT MATRIX DECK
                              hr(),
                              helpText("3. Export Full Multi-Engine Simulation Run Folder Package:"),
                              selectInput("sim_export_destination_channel", "Select Package Export Channel:",
                                          choices = c("Download Compressed Zip to Desktop" = "desktop",
                                                      "Push Package Folder to Microsoft SharePoint" = "sharepoint")),
                              uiOutput("dynamic_sim_folder_export_controls_ui")
                              
                            ),
                            
                            
                            # Right Panel: Code Canvas & Output Performance Display
                            layout_columns(
                              col_widths = c(12, 12),
                              card(
                                card_header("Compiled Simulator Configuration & Script Canvas"),
                                verbatimTextOutput("llm_model_detection_badge"),
                                textAreaInput("compiled_simulation_script_canvas", label = NULL,
                                              value = "# Select a source model and target engine, then click 'Translate & Compile'...",
                                              rows = 12, width = "100%"),
                                card_footer(
                                  layout_columns(
                                    col_widths = c(6, 6),
                                    textAreaInput("native_param_copy_box", label = "Extracted Parameter Estimates Matrix (Natively Copied):", 
                                                  value = "", rows = 2, width = "100%"),
                                    helpText("💡 Note: Structural definitions are classified by AI, while numerical parameter values are extracted natively to ensure maximum precision and zero data exposure.")
                                  )
                                )
                              ),
                              card(
                                card_header("Projected Population Concentration Trajectory Performance"),
                                plotOutput("nonmem_simulation_plot", height = "350px")
                              )
                            )
                          )
                )
              )
    )
    
  )
}
                                  
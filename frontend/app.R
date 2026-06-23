library(shiny)

source("../backend/savings_calculator.R")
source("../backend/loan_calculator.R")

pixel_art_svg <- function(rows, colors, px = 8) {
  rects <- c()
  for (r in seq_along(rows)) {
    chars <- strsplit(rows[r], "")[[1]]
    for (cc in seq_along(chars)) {
      ch <- chars[cc]
      if (ch != "." && !is.null(colors[[ch]])) {
        x    <- (cc - 1) * px
        y    <- (r  - 1) * px
        fill <- colors[[ch]]
        rects <- c(rects, sprintf(
          '<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>',
          x, y, px, px, fill
        ))
      }
    }
  }
  w <- max(nchar(rows)) * px
  h <- length(rows) * px
  paste0(
    sprintf('<svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg" style="image-rendering:pixelated;display:block;">',
      w, h, w, h),
    paste(rects, collapse = ""),
    '</svg>'
  )
}

PIXELS <- list(
  knight = list(
    rows = c(
      "..SSSS..",
      ".SSSSSS.",
      ".SYYSS..",
      ".SYYSS..",
      "..SSSS..",
      "RRRRRRRR",
      "RRGGGGRR",
      "RRGGGGRR",
      "RRRRRRRR",
      ".RRRRRR.",
      ".DD..DD.",
      ".DD..DD.",
      ".DD..DD.",
      ".DD..DD.",
      "DD....DD",
      "DD....DD"
    ),
    colors = list(S="#99aabb", Y="#f4c87e", R="#cc2244", G="#aabbcc", D="#334455")
  ),
  wizard = list(
    rows = c(
      "...PP...",
      "..PPPP..",
      ".PPPPPP.",
      "PPPPPPPP",
      ".YYYYYY.",
      ".YYYYYY.",
      "PPPPPPPP",
      "POOOOOPP",
      "PPPPPPPP",
      "PPPPPPPP",
      ".PPPPPP.",
      ".PP..PP.",
      ".PP..PP.",
      ".PP..PP.",
      "PP....PP",
      "PP....PP"
    ),
    colors = list(P="#9933cc", Y="#f4c87e", O="#ffaa00")
  ),
  guardian = list(
    rows = c(
      "..BBBB..",
      ".BBBBBB.",
      ".BYYYYB.",
      ".BYYYYB.",
      "..BBBB..",
      "GGBBBBGG",
      "BBBBBBBB",
      "BBBBBBBB",
      "BGGGGGBB",
      "BBBBBBBB",
      ".BB..BB.",
      ".BB..BB.",
      ".BB..BB.",
      ".BB..BB.",
      "BBB..BBB",
      "BBB..BBB"
    ),
    colors = list(B="#2244cc", Y="#f4c87e", G="#ffaa00")
  ),
  ranger = list(
    rows = c(
      "..GGGG..",
      ".GGGGGG.",
      ".GYYYYG.",
      ".GYYYYG.",
      "..GGGG..",
      ".GGGGGG.",
      "GGGGGGGG",
      "GWWWWWGG",
      "GGGGGGGG",
      "GGGGGGGG",
      ".GG..GG.",
      ".GG..GG.",
      ".GG..GG.",
      ".GG..GG.",
      "WGG..GGW",
      "WGG..GGW"
    ),
    colors = list(G="#228833", Y="#f4c87e", W="#885522")
  )
)


# ── RPG CLASSES ──────────────────────────────────────────────
CLASSES <- list(
  knight  = list(id="knight",   name="KNIGHT",   title="Debt Crusher",
                 desc="You charge head-first into debt. Aggressive payoff is your weapon.",
                 color="#cc2244"),
  wizard  = list(id="wizard",   name="WIZARD",   title="Wealth Conjurer",
                 desc="You grow wealth through the ancient magic of compound interest.",
                 color="#9933cc"),
  guardian= list(id="guardian", name="GUARDIAN", title="Balance Keeper",
                 desc="Master of saving and debt. The wise, balanced path.",
                 color="#2244cc"),
  ranger  = list(id="ranger",   name="RANGER",   title="Income Hunter",
                 desc="You track every coin and maximize every wage earned.",
                 color="#228833")
)


# ── CSS ──────────────────────────────────────────────────────
rpg_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&family=Share+Tech+Mono&display=swap');

  * { box-sizing: border-box; }
  body {
    background-color: #0a0a0f;
    font-family: 'Share Tech Mono', monospace;
    color: #c8c8ff;
    margin: 0; padding: 0;
  }
  h1,h2,h3,.orbitron { font-family: 'Orbitron', sans-serif; }

  .screen { max-width: 960px; margin: 0 auto; padding: 30px 20px; }

  .rpg-title {
    text-align: center;
    font-family: 'Orbitron', sans-serif;
    font-size: 28px;
    color: #fff;
    text-shadow: 0 0 20px #7755ff, 0 0 40px #5533cc;
    letter-spacing: 6px;
    margin-bottom: 4px;
  }
  .rpg-sub {
    text-align: center;
    font-size: 11px;
    color: #6655aa;
    letter-spacing: 4px;
    margin-bottom: 36px;
  }

  /* class select grid */
  .class-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 16px;
    margin-bottom: 20px;
  }
  .class-card {
    background: #0f0f1e;
    border: 1px solid #2a2a55;
    border-radius: 4px;
    padding: 20px 16px;
    cursor: pointer;
    transition: all 0.15s;
    display: flex;
    align-items: center;
    gap: 16px;
  }
  .class-card:hover {
    border-color: #7755ff;
    box-shadow: 0 0 14px #7755ff33;
  }
  .class-card.selected {
    border-color: #44ff99;
    box-shadow: 0 0 18px #44ff9944;
    background: #0f1f18;
  }
  .class-pixel { flex-shrink: 0; }
  .class-info  { flex: 1; }
  .class-name  {
    font-family: 'Orbitron', sans-serif;
    font-size: 13px;
    letter-spacing: 3px;
    margin-bottom: 3px;
  }
  .class-title { font-size: 11px; color: #44ff99; margin-bottom: 6px; }
  .class-desc  { font-size: 11px; color: #6677aa; line-height: 1.6; }

  /* character builder panel */
  .builder-panel {
    background: #0f0f1e;
    border: 1px solid #2a2a55;
    border-radius: 6px;
    padding: 28px;
    display: flex;
    gap: 32px;
    align-items: flex-start;
    margin-bottom: 20px;
  }
  .builder-sprite {
    flex-shrink: 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 12px;
  }
  .builder-sprite-frame {
    background: #050510;
    border: 2px solid #44ff99;
    border-radius: 4px;
    padding: 16px;
    box-shadow: 0 0 24px #44ff9933, inset 0 0 16px #44ff9911;
  }
  .builder-class-name {
    font-family: 'Orbitron', sans-serif;
    font-size: 11px;
    letter-spacing: 3px;
    text-align: center;
  }
  .builder-class-title {
    font-size: 10px;
    color: #44ff99;
    letter-spacing: 2px;
    text-align: center;
  }
  .builder-fields { flex: 1; }

  /* generic box */
  .rpg-box {
    background: #0f0f1e;
    border: 1px solid #2a2a55;
    border-radius: 4px;
    padding: 22px;
    margin-bottom: 16px;
  }
  .rpg-box-title {
    font-family: 'Orbitron', sans-serif;
    font-size: 11px;
    letter-spacing: 3px;
    color: #7755ff;
    border-bottom: 1px solid #2a2a55;
    padding-bottom: 10px;
    margin-bottom: 18px;
  }

  /* inputs */
  .form-control {
    background: #06060f !important;
    border: 1px solid #333366 !important;
    color: #c8c8ff !important;
    font-family: 'Share Tech Mono', monospace !important;
    border-radius: 3px !important;
  }
  .form-control:focus {
    border-color: #44ff99 !important;
    box-shadow: 0 0 8px #44ff9933 !important;
  }
  label { color: #7766bb; font-size: 12px; letter-spacing: 1px; }

  /* buttons */
  .btn-rpg {
    background: transparent;
    color: #44ff99;
    border: 1px solid #44ff99;
    font-family: 'Orbitron', sans-serif;
    font-size: 12px;
    letter-spacing: 3px;
    padding: 12px 24px;
    width: 100%;
    cursor: pointer;
    transition: all 0.2s;
    margin-top: 10px;
  }
  .btn-rpg:hover {
    background: #44ff99;
    color: #050510;
    box-shadow: 0 0 20px #44ff9966;
  }

  /* tabs */
  .nav-tabs { border-bottom: 1px solid #2a2a55 !important; margin-bottom: 20px; }
  .nav-tabs > li > a {
    font-family: 'Orbitron', sans-serif !important;
    font-size: 10px !important;
    letter-spacing: 2px !important;
    color: #554488 !important;
    background: transparent !important;
    border: none !important;
    border-bottom: 2px solid transparent !important;
    padding: 10px 14px !important;
  }
  .nav-tabs > li.active > a, .nav-tabs > li > a:hover {
    color: #44ff99 !important;
    border-bottom-color: #44ff99 !important;
    background: transparent !important;
  }

  /* output */
  pre.shiny-text-output {
    background: #06060f !important;
    border: 1px solid #2a2a55 !important;
    color: #44ff99 !important;
    font-family: 'Share Tech Mono', monospace !important;
    font-size: 12px !important;
    line-height: 1.9 !important;
    padding: 18px !important;
    border-radius: 4px !important;
    min-height: 160px;
  }

  /* stat bars */
  .stat-row { margin-bottom: 14px; }
  .stat-label {
    font-size: 11px; color: #7766bb; letter-spacing: 2px;
    margin-bottom: 4px; display: flex; justify-content: space-between;
  }
  .stat-bar-bg {
    background: #111128; border: 1px solid #2a2a55;
    border-radius: 2px; height: 10px; overflow: hidden;
  }
  .stat-bar-fill { height: 100%; border-radius: 2px; transition: width 0.5s ease; }

  /* char sheet */
  .char-header { display: flex; align-items: center; gap: 20px; margin-bottom: 24px; }
  .char-avatar {
    background: #050510; border: 2px solid #44ff99;
    border-radius: 6px; padding: 10px;
    box-shadow: 0 0 16px #44ff9933;
  }
  .char-name  { font-family: 'Orbitron', sans-serif; font-size: 20px; color: #fff; }
  .char-class { font-size: 12px; color: #44ff99; letter-spacing: 2px; margin-top: 4px; }
  .char-level { font-size: 12px; color: #ffcc44; margin-top: 4px; }

  /* portfolio */
  .port-table { width: 100%; border-collapse: collapse; font-size: 12px; }
  .port-table th {
    color: #7755ff; letter-spacing: 2px; font-size: 10px;
    border-bottom: 1px solid #2a2a55; padding: 8px 10px; text-align: left;
  }
  .port-table td { padding: 8px 10px; border-bottom: 1px solid #111128; color: #c8c8ff; }
  .port-table tr:hover td { background: #111128; }
  .badge-loan    { color: #ff4455; }
  .badge-savings { color: #44ff99; }
  .err-msg { color: #ff4455; font-size: 12px; margin-top: 6px; }

  ::-webkit-scrollbar { width: 4px; }
  ::-webkit-scrollbar-track { background: #0a0a0f; }
  ::-webkit-scrollbar-thumb { background: #44ff99; }
"


# ── SAVE / LOAD ──────────────────────────────────────────────
save_dir  <- "data"
save_file <- file.path(save_dir, "player.rds")
save_player <- function(p) { if (!dir.exists(save_dir)) dir.create(save_dir); saveRDS(p, save_file) }
load_player <- function()  { if (file.exists(save_file)) readRDS(save_file) else NULL }

calc_stats <- function(player) {
  wages         <- player$wages
  total_savings <- if (length(player$savings) == 0) 0 else sum(sapply(player$savings, function(s) s$balance))
  total_debt    <- if (length(player$loans)   == 0) 0 else sum(sapply(player$loans,   function(l) l$balance))
  savings_rate  <- if (wages > 0) min(total_savings / (wages * 12), 1) else 0
  debt_ratio    <- if (wages > 0) min(total_debt    / (wages * 12), 2) / 2 else 0
  hp            <- max(0, round((1 - debt_ratio) * 100))
  xp            <- round(savings_rate * 100)
  list(total_savings=total_savings, total_debt=total_debt,
       hp=hp, xp=xp, level=max(1, min(50, round((hp+xp)/4))))
}


# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(rpg_css))),
  uiOutput("screen")
)


# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    page           = "select",
    selected_class = NULL,
    player         = load_player()
  )

  observe({ if (!is.null(rv$player)) rv$page <- "dashboard" })

  output$screen <- renderUI({
    if      (rv$page == "select")    screen_select()
    else if (rv$page == "create")    screen_create()
    else if (rv$page == "dashboard") screen_dashboard(rv$player)
  })


  # ── SCREEN 1: CLASS SELECT ───────────────────────────────
  screen_select <- function() {
    div(class = "screen",
      br(),
      div(class = "rpg-title", "⚔ FINANCE QUEST ⚔"),
      div(class = "rpg-sub", "// CHOOSE YOUR CLASS //"),

      div(class = "rpg-box",
        div(class = "rpg-box-title", "> SELECT CLASS"),
        div(class = "class-grid",
          lapply(CLASSES, function(cls) {
            px_data   <- PIXELS[[cls$id]]
            sprite    <- pixel_art_svg(px_data$rows, px_data$colors, px = 9)
            is_sel    <- !is.null(rv$selected_class) && rv$selected_class == cls$id
            div(
              class   = paste("class-card", if (is_sel) "selected" else ""),
              onclick = sprintf("Shiny.setInputValue('pick_class','%s',{priority:'event'})", cls$id),
              div(class = "class-pixel", HTML(sprite)),
              div(class = "class-info",
                div(class = "class-name",  style = paste0("color:", cls$color), cls$name),
                div(class = "class-title", cls$title),
                div(class = "class-desc",  cls$desc)
              )
            )
          })
        ),
        tags$button("▶  SELECT CHARACTER", class = "btn-rpg",
          onclick = "Shiny.setInputValue('confirm_class', Math.random())")
      )
    )
  }

  observeEvent(input$pick_class,    { rv$selected_class <- input$pick_class })
  observeEvent(input$confirm_class, { req(rv$selected_class); rv$page <- "create" })


  # ── SCREEN 2: CHARACTER CREATE ───────────────────────────
  screen_create <- function() {
    cls     <- CLASSES[[rv$selected_class]]
    px_data <- PIXELS[[rv$selected_class]]
    sprite  <- pixel_art_svg(px_data$rows, px_data$colors, px = 14)

    div(class = "screen",
      br(),
      div(class = "rpg-title", "⚔ FINANCE QUEST ⚔"),
      div(class = "rpg-sub", "// BUILD YOUR ADVENTURER //"),

      div(class = "builder-panel",

        # Left: pixel character display
        div(class = "builder-sprite",
          div(class = "builder-sprite-frame", HTML(sprite)),
          div(class = "builder-class-name",  style = paste0("color:", cls$color), cls$name),
          div(class = "builder-class-title", cls$title)
        ),

        # Right: form fields
        div(class = "builder-fields",
          div(class = "rpg-box-title", "> ADVENTURER DETAILS"),
          textInput("player_name",   "// ADVENTURER NAME",    placeholder = "Enter your name..."),
          numericInput("player_wages", "// MONTHLY WAGES ($)", value = NULL, min = 0),
          uiOutput("create_error"),
          tags$button("▶  BEGIN JOURNEY", class = "btn-rpg",
            onclick = "Shiny.setInputValue('begin_quest', Math.random())")
        )
      )
    )
  }

  output$create_error <- renderUI({ NULL })

  observeEvent(input$begin_quest, {
    name  <- trimws(input$player_name)
    wages <- input$player_wages
    if (nchar(name) == 0 || is.na(wages) || wages <= 0) {
      output$create_error <- renderUI({
        div(class = "err-msg", "[ ERROR ] >> Please fill in all fields.")
      })
      return()
    }
    rv$player <- list(name=name, class=rv$selected_class, wages=wages, savings=list(), loans=list())
    save_player(rv$player)
    rv$page <- "dashboard"
  })


  # ── SCREEN 3: DASHBOARD ──────────────────────────────────
  screen_dashboard <- function(player) {
    cls     <- CLASSES[[player$class]]
    px_data <- PIXELS[[player$class]]
    sprite  <- pixel_art_svg(px_data$rows, px_data$colors, px = 8)
    stats   <- calc_stats(player)

    div(class = "screen",
      br(),
      div(class = "rpg-title", "⚔ FINANCE QUEST ⚔"),
      div(class = "rpg-sub", paste0("// WELCOME BACK, ", toupper(player$name), " //"),
          style = "margin-bottom:20px"),

      tabsetPanel(

        # CHARACTER SHEET
        tabPanel("🧾 CHARACTER", br(),
          div(class = "rpg-box",
            div(class = "char-header",
              div(class = "char-avatar", HTML(sprite)),
              div(
                div(class = "char-name",  player$name),
                div(class = "char-class", style=paste0("color:",cls$color),
                    paste0(cls$name, " — ", cls$title)),
                div(class = "char-level", paste0("✦ LEVEL ", stats$level, " ADVENTURER"))
              )
            ),
            div(class="stat-row",
              div(class="stat-label", span("❤️ FINANCIAL HEALTH"), span(paste0(stats$hp,"/100"))),
              div(class="stat-bar-bg",
                div(class="stat-bar-fill", style=paste0("width:",stats$hp,"%;background:#ff4455;")))
            ),
            div(class="stat-row",
              div(class="stat-label", span("⭐ SAVINGS XP"), span(paste0(stats$xp,"/100"))),
              div(class="stat-bar-bg",
                div(class="stat-bar-fill", style=paste0("width:",stats$xp,"%;background:#ffcc44;")))
            ),
            hr(style="border-color:#2a2a55;margin:16px 0;"),
            fluidRow(
              column(4, div(class="rpg-box", style="text-align:center;padding:14px;",
                div(style="font-size:10px;color:#7766bb;letter-spacing:2px;", "MONTHLY WAGES"),
                div(style="font-size:20px;color:#44ff99;margin-top:6px;",
                    paste0("$", format(player$wages, big.mark=",")))
              )),
              column(4, div(class="rpg-box", style="text-align:center;padding:14px;",
                div(style="font-size:10px;color:#7766bb;letter-spacing:2px;", "TOTAL SAVINGS"),
                div(style="font-size:20px;color:#4488ff;margin-top:6px;",
                    paste0("$", format(round(stats$total_savings), big.mark=",")))
              )),
              column(4, div(class="rpg-box", style="text-align:center;padding:14px;",
                div(style="font-size:10px;color:#7766bb;letter-spacing:2px;", "TOTAL DEBT"),
                div(style="font-size:20px;color:#ff4455;margin-top:6px;",
                    paste0("$", format(round(stats$total_debt), big.mark=",")))
              ))
            )
          )
        ),

        # LOAN CALCULATOR
        tabPanel("⚔️ LOAN CALC", br(),
          fluidRow(
            column(5, div(class="rpg-box",
              div(class="rpg-box-title", "> LOAN PARAMETERS"),
              numericInput("l_principal", "// LOAN AMOUNT ($)",    value=NULL, min=0),
              numericInput("l_rate",      "// ANNUAL RATE (%)",    value=NULL, min=0, step=0.1),
              numericInput("l_years",     "// TERM (YEARS)",       value=NULL, min=1),
              tags$button("▶  CALCULATE", class="btn-rpg",
                onclick="Shiny.setInputValue('calc_loan',Math.random())")
            )),
            column(7, div(class="rpg-box",
              div(class="rpg-box-title", "> OUTPUT"),
              verbatimTextOutput("loan_result")
            ))
          )
        ),

        # SAVINGS CALCULATOR
        tabPanel("🪙 SAVINGS CALC", br(),
          fluidRow(
            column(5, div(class="rpg-box",
              div(class="rpg-box-title", "> SAVINGS PARAMETERS"),
              numericInput("s_principal",    "// INITIAL DEPOSIT ($)",      value=NULL, min=0),
              numericInput("s_rate",         "// ANNUAL RATE (%)",          value=NULL, min=0, step=0.1),
              numericInput("s_years",        "// TERM (YEARS)",             value=NULL, min=1),
              numericInput("s_contribution", "// MONTHLY CONTRIBUTION ($)", value=0,    min=0),
              tags$button("▶  CALCULATE", class="btn-rpg",
                onclick="Shiny.setInputValue('calc_savings',Math.random())")
            )),
            column(7, div(class="rpg-box",
              div(class="rpg-box-title", "> OUTPUT"),
              verbatimTextOutput("savings_result")
            ))
          )
        ),

        # PORTFOLIO
        tabPanel("📦 PORTFOLIO", br(),
          fluidRow(
            column(6, div(class="rpg-box",
              div(class="rpg-box-title", "> ADD SAVINGS ACCOUNT"),
              textInput("ps_label",      "// LABEL",        placeholder="e.g. Emergency Fund"),
              numericInput("ps_balance", "// BALANCE ($)",  value=NULL, min=0),
              tags$button("+ ADD", class="btn-rpg",
                onclick="Shiny.setInputValue('add_savings',Math.random())")
            )),
            column(6, div(class="rpg-box",
              div(class="rpg-box-title", "> ADD LOAN"),
              textInput("pl_label",      "// LABEL",            placeholder="e.g. Car Loan"),
              numericInput("pl_balance", "// BALANCE ($)",      value=NULL, min=0),
              numericInput("pl_rate",    "// INTEREST RATE (%)",value=NULL, min=0, step=0.1),
              tags$button("+ ADD", class="btn-rpg",
                onclick="Shiny.setInputValue('add_loan',Math.random())")
            ))
          ),
          div(class="rpg-box",
            div(class="rpg-box-title", "> MY ACCOUNTS"),
            uiOutput("portfolio_table")
          )
        )
      )
    )
  }


  # ── CALCULATOR OUTPUTS ───────────────────────────────────
  output$loan_result <- renderText({
    req(input$calc_loan)
    p <- input$l_principal; r <- input$l_rate; y <- as.integer(input$l_years)
    if (is.na(p)||is.na(r)||is.na(y)) return("[ ERROR ] >> Fill in all fields.")
    M  <- calculate_monthly_payment(p, r, y)
    tp <- round(M * y * 12, 2)
    ti <- round(tp - p, 2)
    paste0("[ LOAN CALCULATOR ]\n","------------------------------------\n",
      sprintf("  PRINCIPAL      : $%s\n", format(p,  big.mark=",")),
      sprintf("  INTEREST RATE  : %.2f%%\n", r),
      sprintf("  TERM           : %d years\n", y),
      "------------------------------------\n",
      sprintf("  MONTHLY PMT    : $%s\n", format(M,  big.mark=",")),
      sprintf("  TOTAL PAID     : $%s\n", format(tp, big.mark=",")),
      sprintf("  TOTAL INTEREST : $%s\n", format(ti, big.mark=",")),
      "------------------------------------\n","[ STATUS ] >> COMPLETE ✓")
  })

  output$savings_result <- renderText({
    req(input$calc_savings)
    p <- input$s_principal; r <- input$s_rate; y <- as.integer(input$s_years)
    contrib <- if (is.na(input$s_contribution)) 0 else input$s_contribution
    if (is.na(p)||is.na(r)||is.na(y)) return("[ ERROR ] >> Fill in all fields.")
    final   <- calculate_savings(p, r, y, contrib)
    total_c <- p + (contrib * 12 * y)
    earned  <- round(final - total_c, 2)
    paste0("[ SAVINGS CALCULATOR ]\n","------------------------------------\n",
      sprintf("  INITIAL DEPOSIT  : $%s\n",   format(p,       big.mark=",")),
      sprintf("  MONTHLY CONTRIB  : $%s\n",   format(contrib, big.mark=",")),
      sprintf("  INTEREST RATE    : %.2f%%\n", r),
      sprintf("  TERM             : %d years\n", y),
      "------------------------------------\n",
      sprintf("  FINAL BALANCE    : $%s\n",   format(final,   nsmall=2, big.mark=",")),
      sprintf("  TOTAL CONTRIBUTED: $%s\n",   format(total_c, big.mark=",")),
      sprintf("  INTEREST EARNED  : $%s\n",   format(earned,  nsmall=2, big.mark=",")),
      "------------------------------------\n","[ STATUS ] >> COMPLETE ✓")
  })


  # ── PORTFOLIO ────────────────────────────────────────────
  observeEvent(input$add_savings, {
    req(rv$player)
    label <- trimws(input$ps_label); bal <- input$ps_balance
    if (nchar(label)==0 || is.na(bal) || bal < 0) return()
    rv$player$savings <- c(rv$player$savings, list(list(label=label, balance=bal)))
    save_player(rv$player)
  })

  observeEvent(input$add_loan, {
    req(rv$player)
    label <- trimws(input$pl_label); bal <- input$pl_balance; rate <- input$pl_rate
    if (nchar(label)==0 || is.na(bal) || bal < 0) return()
    rv$player$loans <- c(rv$player$loans,
      list(list(label=label, balance=bal, rate=ifelse(is.na(rate),0,rate))))
    save_player(rv$player)
  })

  output$portfolio_table <- renderUI({
    req(rv$player)
    rows <- list()
    for (s in rv$player$savings)
      rows <- c(rows, list(tags$tr(
        tags$td(class="badge-savings","🪙 SAVINGS"), tags$td(s$label),
        tags$td(paste0("$", format(s$balance, big.mark=",")))
      )))
    for (l in rv$player$loans)
      rows <- c(rows, list(tags$tr(
        tags$td(class="badge-loan","⚔️ LOAN"), tags$td(l$label),
        tags$td(paste0("$", format(l$balance, big.mark=",")))
      )))
    if (length(rows)==0)
      return(div(style="color:#333366;font-size:12px;","[ NO ACCOUNTS ADDED YET ]"))
    tags$table(class="port-table",
      tags$thead(tags$tr(tags$th("TYPE"), tags$th("LABEL"), tags$th("BALANCE"))),
      tags$tbody(rows))
  })

}

shinyApp(ui, server)
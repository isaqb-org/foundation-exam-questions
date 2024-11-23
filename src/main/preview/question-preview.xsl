<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema">
    
    <xsl:output method="html" indent="yes" encoding="UTF-8"/>
    
    <!-- Parameter for CSS file location -->
    <xsl:param name="css-file" as="xs:string" required="yes"/>
    
    <!-- Main template -->
    <xsl:template match="/">
        <html>
            <head>
                <meta charset="UTF-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                <title>
                    <xsl:value-of select="*//@id"/> - Question Preview
                </title>
                <style>
                    <!-- Load and insert CSS -->
                    <xsl:value-of select="unparsed-text($css-file)"/>
                </style>
            </head>
            <body>
                <main class="container">
                    <xsl:apply-templates select="pickQuestion|categoryQuestion"/>
                </main>
            </body>
        </html>
    </xsl:template>
    
    <!-- Pick Question Template -->
    <xsl:template match="pickQuestion">
        <article class="question">
            <!-- Header -->
            <header class="question-header">
                <div>
                    <h1 class="question-title">
                        <xsl:value-of select="@id"/>
                        (<xsl:value-of select="@points"/> point<xsl:if test="@points > 1">s</xsl:if>)
                    </h1>
                    <div class="learning-goals">
                        <xsl:apply-templates select="refersToLgs/lg"/>
                    </div>
                </div>
                <div class="question-type">Pick Question</div>
            </header>
            
            <!-- Question Text -->
            <section class="question-section">
                <h2 class="section-title">Question Text:</h2>
                <div class="question-stem">
                    <xsl:apply-templates select="stem/text"/>
                </div>
            </section>
            
            <!-- Options -->
            <section class="question-section">
                <h2 class="section-title">Options:</h2>
                <div class="options">
                    <xsl:apply-templates select="pickOptions/option"/>
                </div>
            </section>
            
            <!-- Explanation if present -->
            <xsl:if test="explanation">
                <section class="question-section">
                    <h2 class="section-title">Explanation:</h2>
                    <div class="explanation">
                        <xsl:apply-templates select="explanation/text"/>
                    </div>
                </section>
            </xsl:if>
        </article>
    </xsl:template>
    
    <!-- Category Question Template -->
    <xsl:template match="categoryQuestion">
        <article class="question">
            <!-- Header similar to pickQuestion -->
            <header class="question-header">
                <div>
                    <h1 class="question-title">
                        <xsl:value-of select="@id"/>
                        (<xsl:value-of select="@points"/> point<xsl:if test="@points > 1">s</xsl:if>)
                    </h1>
                    <div class="learning-goals">
                        <xsl:apply-templates select="refersToLgs/lg"/>
                    </div>
                </div>
                <div class="question-type">Category Question</div>
            </header>
            
            <!-- Question Text -->
            <section class="question-section">
                <h2 class="section-title">Question Text:</h2>
                <div class="question-stem">
                    <xsl:apply-templates select="stem/text"/>
                </div>
            </section>
            
            <!-- Categories -->
            <section class="question-section">
                <h2 class="section-title">Categories:</h2>
                <div class="categories">
                    <xsl:apply-templates select="categoryStatements/categories/category"/>
                </div>
            </section>
            
            <!-- Statements -->
            <section class="question-section">
                <h2 class="section-title">Statements:</h2>
                <div class="statements">
                    <xsl:apply-templates select="categoryStatements/statements/statement"/>
                </div>
            </section>
            
            <!-- Explanation if present -->
            <xsl:if test="explanation">
                <section class="question-section">
                    <h2 class="section-title">Explanation:</h2>
                    <div class="explanation">
                        <xsl:apply-templates select="explanation/text"/>
                    </div>
                </section>
            </xsl:if>
        </article>
    </xsl:template>
    
    <!-- Helper templates -->
    <xsl:template match="lg">
        <span class="learning-goal">
            <xsl:value-of select="@curriculumVersion"/>
            <xsl:text> lg-</xsl:text>
            <xsl:value-of select="@lg"/>
        </span>
    </xsl:template>
    
    <xsl:template match="text">
        <div class="language-text {@xml:lang}">
            <span class="lang-label">
                <xsl:value-of select="@xml:lang"/>:
            </span>
            <xsl:value-of select="."/>
        </div>
    </xsl:template>
    
    <xsl:template match="option">
        <div class="option">
            <xsl:attribute name="data-correct">
                <xsl:choose>
                    <xsl:when test="@correct">true</xsl:when>
                    <xsl:otherwise>false</xsl:otherwise>
                </xsl:choose>
            </xsl:attribute>
            <xsl:attribute name="data-identifier">
                <xsl:value-of select="@identifier"/>
            </xsl:attribute>
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>
    
    <xsl:template match="category">
        <div class="category" data-label="{@label}">
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>
    
    <xsl:template match="statement">
        <div class="statement" data-category="{@correctCategory}" data-identifier="{@identifier}">
            <xsl:apply-templates select="text"/>
        </div>
    </xsl:template>
</xsl:stylesheet>
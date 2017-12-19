#-------------------------------------------------
#
# Project created by QtCreator 2017-11-02T12:58:03
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = imageanalysis
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp

HEADERS  += mainwindow.h \
    histoclass.h

FORMS    += mainwindow.ui

INCLUDEPATH += C:\opencv\build\include \

LIBS += -LC:\opencv\build\x64\vc14\lib \
-lopencv_world310 \

#ifndef C_SRC_CRITICAL_SECTION_H_
#define C_SRC_CRITICAL_SECTION_H_

#include "erl_nif.h"
#include "macros.h"

class CriticalSection
{
public:

    CriticalSection() { }
    virtual ~CriticalSection() {}

    virtual void Enter() = 0;
    virtual void Leave() = 0;
};

class NullCriticalSection : public CriticalSection
{
public:

    NullCriticalSection() {}
    ~NullCriticalSection() {}

    void Enter() override {}
    void Leave() override {}

private:

    DISALLOW_COPY_AND_ASSIGN(NullCriticalSection);
};

class EnifCriticalSection : public CriticalSection
{
public:

    EnifCriticalSection() { mutex_ = enif_mutex_create(NULL);}
    ~EnifCriticalSection() {enif_mutex_destroy(mutex_);}

    void Enter() override {enif_mutex_lock(mutex_);}
    void Leave() override {enif_mutex_unlock(mutex_);}

private:

    ErlNifMutex *mutex_;
    DISALLOW_COPY_AND_ASSIGN(EnifCriticalSection);
};

class CritScope
{
public:

    explicit CritScope(CriticalSection *pp) : pcrit_(pp) { pcrit_->Enter();}
    ~CritScope() {pcrit_->Leave();}

private:

    CriticalSection *pcrit_;
    DISALLOW_COPY_AND_ASSIGN(CritScope);
};

#endif  // C_SRC_CRITICAL_SECTION_H_

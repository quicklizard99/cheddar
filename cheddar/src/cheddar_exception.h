#ifndef CHEDDAREXCEPTION_H
#define CHEDDAREXCEPTION_H

#include <string>

class CheddarException : public std::exception
{
public:
  CheddarException(const char *m) : msg_(m) {}
  virtual ~CheddarException() throw() {}

  virtual const char* what() const throw()
  {
     return (msg_.c_str());
  }

private:
  std::string msg_;
};

#endif

